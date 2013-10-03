;;; ox-blogit.el ---

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html presentation
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

;;; Dependencies

(require 'ox-html)
(require 'ox-publish)
(require 'ht)
(require 'dash)
(require 's)
(require 'mustache)

(eval-when-compile (require 'cl))

(defconst blogit-version "0.1"
  "Blogit version string.")

(defconst blogit-url
  "http://github.com/coldnew/emacs-blogit"
  "Url for blogit.")


;;; User Configuration Variables

(defgroup blogit nil
  "Options for generating static pages using blogit."
  :tag "Org static page generator" :group 'org)

(defcustom blogit-source-dir nil
  "The source directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-output-dir nil
  "The output directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-cache-dir
  (concat blogit-output-dir "/.cache")
  "The cache directory for blogit.")

(defcustom blogit-google-analytics-id nil
  "Personal google analytics id."
  :group 'blogit :type 'string)

(defcustom blogit-disqus-shortname nil
  "Personal disqus shortname."
  :group 'blogit :type 'string)

(defcustom blogit-template-dir "templates/"
  "Template directory."
  :group 'blogit :type 'string)

(setq blogit-project-list
      `("blogit"
        :base-directory ,blogit-source-dir
        :publishing-directory ,blogit-output-dir
        :publishing-function org-blogit-publish-to-html
        :recursive t
        ))

(defvar blogit-template-list
  '((:page_header      . "page_header.html")
    (:page_navigator   . "page_navigator.html")
    (:page_footer      . "page_footer.html")
    (:index            . "index.html")
    (:blog_post        . "blog_post.html")
    (:blog_index       . "blog_index.html")
    (:blog_rss         . "blog_rss.html")
    (:plugin_analytics . "plugin_analytics.html")
    (:plugin_disqus    . "plugin_disqus.html")
    (:newpost          . "newpost.org")
    )
  "Template filename define for blogit to parse.")


;;; Internal functions

(defun blogit--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun blogit--string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major
mode, format the string with MODE's format settings."
  (with-temp-buffer
    (insert string)
    (set-buffer-file-coding-system 'utf-8-unix)
    (when (and mode (functionp mode))
      (funcall mode)
      (flush-lines "^[ \\t]*$" (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max))
      (indent-region (point-min) (point-max)))
    (when (file-writable-p file)
      (write-region (point-min) (point-max) file))))

;; FIXME:
(defun blogit--template-to-string (file)
  "Read the content of FILE in template dir and return it as string."
  (blogit--file-to-string file))

;; FIXME: modify blogit-template-dir
(defun blogit--get-template (key)
  "Get match template filename with fullpath according to key."
  (convert-standard-filename
   (concat blogit-source-dir "/" blogit-template-dir
           (cdr (assoc key blogit-template-list)))))

(defun blogit--render-template (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (blogit--get-template type) type)))
    (mustache-render (blogit--template-to-string file) context)))

(defun blogit--parse-option (info key)
  "Read option value of org file opened in current buffer.

This function will first use the standard way to parse org-option.
If parsing failed, use regexp to get the options, else return nil.
"
  (flet ((plist-get-str (info key)
                        (let ((r (plist-get info key)))
                          (if (stringp r) r (or (car r) "")))))
    (let* ((keystr1 (symbol-name key))
           (keystr (upcase (s-right (- (length keystr1) 1) keystr1)))
           (match-regexp (org-make-options-regexp `(,keystr)))
           (option (plist-get-str info key)))

      ;; if we can use plist-get to get org-option, use it
      ;; else use regexp to find options
      (or (if (not (string= "" option)) option)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward match-regexp nil t)
              (match-string-no-properties 2 nil)))))))

(defun blogit--modify-org-option (option value)
  "Modify option value of org file opened in current buffer.
If option does not exist, create it automatically."
  (let ((match-regexp (org-make-options-regexp `(,option)))
        (blank-regexp "^#\\+\\(\\w*\\):[        ]*\\(.*\\)")
        (insert-option '(insert (concat "#+" option ": " value)))
        (mpoint))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (progn
            (goto-char (point-at-bol))
            (kill-line)
            (eval insert-option))
        ;; no option found, insert it
        (progn
          (goto-char (point-min))
          (while (re-search-forward blank-regexp nil t)
            (setq mpoint (point)))
          (if (not mpoint) (setq mpoint (point-min)))
          (goto-char mpoint)
          (when (not (= mpoint (point-min)))
            (goto-char (point-at-eol))
            (newline-and-indent))
          (eval insert-option)
          (if (= mpoint (point-min))
              (newline-and-indent))
          )))))

(defmacro blogit--build-context (info &rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)

This function is used to create context for blogit-render function,
many useful context is predefined here, but you can overwrite it.
"
  `(ht
    ("BLOGIT" (concat "emacs-blogit ver." blogit-version))
    ("BLOGIT_URL" (format "<a href=\"%s\"> emacs-blogit </a>" blogit-url))
    ("MAIN_TITLE" (or blogit-site-main-title ""))
    ("SUB_TITLE"  (or blogit-site-sub-title ""))
    ("TITLE"  (or (blogit--parse-option :title) "Untitled"))
    ("AUTHOR" (or (blogit--parse-option info :author) user-full-name "Unknown"))
    ("EMAIL" (or (blogit--parse-option :email) user-mail-address ""))
    ("DATE" (or (blogit--parse-option :date) ""))
    ("LANGUAGE" (or (blogit--parse-option :language) "en"))
    ("DESCRIPTION" (or (blogit--parse-option info :description) ""))
    ,@pairs))


;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit 'html
  :options-alist
  '(
    (:analytics         "ANALYTICS"         nil   nil   t)
    (:disqus            "DISQUS"            nil   nil   t)

    ;; disable org-html default style
    (:html-head-include-default-style nil "html-style" nil)
    (:html-head-include-scripts nil "html-scripts" nil)
    )

  :translate-alist
  '((template     . org-blogit-template)))

(defun blogit--build-header-info (info)
  (blogit--render-template :page_header (blogit--build-context info)))

(defun org-blogit-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((meta-info (org-html--build-meta-info info)))
    ;; we override the org-html--build-meta-info to insert our
    ;; html header.
    (flet ((org-html--build-meta-info (info)
                                      (concat
                                       meta-info
                                       (blogit--build-header-info info))))
      (org-html-template contents info))
    ))


;;; End-user functions

;;;###autoload
(defun blogit-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

Export is done in a buffer named \"*Org HTML5 Slide Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
                    'blogit "*Blogit Export*"
                  subtreep visible-only body-only ext-plist))
        (org-export-coding-system org-html-coding-system))
    ;; Set major mode.
    (with-current-buffer outbuf (set-auto-mode t))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

;;;###autoload
(defun org-blogit-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blogit filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))

;;;###autoload
(defun blogit-publish-blog ()
  (interactive)
  (let* ((org-publish-timestamp-directory
          (convert-standard-filename (concat blogit-cache-dir "/")))
         (org-publish-cache nil))
    (org-publish-project blogit-project-list)))

;;;###autoload
(defun blogit-republish-blog ()
  (interactive)
  (let* ((org-publish-timestamp-directory
          (convert-standard-filename (concat blogit-cache-dir "/")))
         (org-publish-cache nil))
    (if (file-exists-p org-publish-timestamp-directory)
        (delete-directory org-publish-timestamp-directory t nil))
    (org-publish-project blogit-project-list)))
