;;; blogit.el --- .

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
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

;;
;;
;;

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install blogit
;;
;; In your .emacs
;;
;;      (require 'blogit)

;;; Code:

(require 'ox)
(require 'ht)
(require 'dash)
(require 's)
(require 'mustache)

;; import extra files in blogit
(mapcar (lambda (x) (require (intern (format "blogit-%s" x)) nil t))
        '("utils" "vars" "create" "test" "export"))


(defconst blogit-version "0.1"
  "Blogit version string.")

(defconst blogit-generator-string
  (concat "emacs-blogit ver. " blogit-version)
  "Generator string to indicate blogit version.")

;;; Customize Variables

(defgroup blogit nil
  "Options for generating static pages using blogit."
  :tag "Org static page generator" :group 'org)

(defcustom blogit-source-dir nil
  "The source directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-output-dir nil
  "The output directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-site-domain nil
  "The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned."
  :group 'blogit :type 'string)

(defcustom blogit-default-language "en"
  "The default language for this site. This value will be set to `en' by default."
  :group 'blogit :type 'string)

(defcustom blogit-date-format "%Y/%m/%d %I:%M %p"
  "Format for printing a date in the sitemap.
See `format-time-string' for allowed formatters."
  :group 'blogit :type 'string)

(defcustom blogit-google-analytics-id nil
  "Personal google analytics id."
  :group 'blogit :type 'string)

(defcustom blogit-disqus-shortname nil
  "Personal disqus shortname."
  :group 'blogit :type 'string)

;;; Templates

(defcustom blogit-template-dir "templates/"
  "Template directory."
  :group 'blogit :type 'string)

(defvar blogit-template-list
  '((:page_header      . "page_header.html")
    (:page_footer      . "page_footer.html")
    (:index            . "index.html")
    (:blog_post        . "blog_post.html")
    (:blog_rss         . "blog_rss.html")
    (:plugin_analytics . "plugin_analytics.html")
    (:plugin_disqus    . "plugin_disqus.html")
    (:newpost          . "newpost.org")
    )
  "Template filename define for blogit to parse.")

;;;; Internal functions

(defun blogit-get-template (key)
  "Get match template filename according to key."
  (cdr (assoc key blogit-template-list)))

(defun blogit-file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun blogit-string-to-file (string file &optional mode)
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

(defun blogit-template-to-string (file)
  "Read the content of FILE in template dir and return it as string."
  (blogit-file-to-string (concat blogit-template-dir file)))

(defun blogit-template-render (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (blogit-get-template type) type)))
    (mustache-render (blogit-template-to-string file) context)))

(defun blogit-parse-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun blogit-modify-option (option value)
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

(defun blogit-sanitize-string (s)
  "Sanitize string S by:

- converting all charcters to pure ASCII
- replacing non alphanumerical by the first char of sha1 algorithm
- downcasing all letters
- trimming leading and tailing \"_\"

This function is used to generate blog post url if not specified."
  (loop for c across s
        with cd
        with gc
        with ret
        do (progn
             (setf gc (get-char-code-property c 'general-category))
             (setf cd (get-char-code-property c 'decomposition)))
        if (or (member gc '(Lu Ll Nd)) (= ?_ c) (= ?- c))
        collect (downcase
                 (char-to-string (if cd (car cd)  c)))
        into ret
        else if (member gc '(Zs))
        collect "_" into ret
        else if (member gc '(Lo))
        collect (s-left 2 (sha1 (char-to-string (if cd (car cd) c))))
        into ret
        finally return (replace-regexp-in-string
                        "--+" "_"
                        (replace-regexp-in-string
                         "^_+\\|_+$" ""
                         (mapconcat 'identity ret "")))))

(defun blogit~~generate-dir-string (date-str yn mn dn)
  "Helper function to regenerte date string fot `blogit-generate-dir-string'."
  (concat (format "%04d" (string-to-int (match-string yn date-str))) "/"
          (format "%02d" (string-to-int (match-string mn date-str))) "/"
          (format "%02d" (string-to-int (match-string dn date-str)))))

(defun blogit-generate-dir-string (date-string)
  "Returns yyyy/mm/dd format of date-string
For examples:
   [Nov. 28, 1994]     => [1994/11/28]
   [November 28, 1994] => [1994/11/28]
   [11/28/1994]        => [1994/11/28]
Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.

This function is used to create directory for new blog post.
"
  (let ((date-str date-string)
        date-list year month date yyyy mm dd)
    (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str))
    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match
       "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9][0-9][0-9][0-9]\\)" date-str)
      (blogit~~generate-dir-string date-str 3 1 2))

     ;; yyyy/mm/dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)/\\([0-9]+\\)/\\([0-9]+\\)" date-str)
      (blogit~~generate-dir-string date-str 1 2 3))

     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9]+\\)-\\([0-9]+\\)" date-str)
      (blogit~~generate-dir-string date-str 1 2 3))

     (t (progn
          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))
          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "/" mm "/" dd))))))

(defun blogit-path-to-root (path)
  "Return path to site root.
ex:
    (root)/2013/12/23/test.html -> ../../..
    (root)/theme                -> ..
"
  (let* ((root (expand-file-name blogit-output-dir))
         (rpath (file-relative-name
                 (expand-file-name path) root))
         (dpath (directory-file-name
                 (or (file-name-directory rpath) "")))
         (nroot "")
         npath)
    ;; if dpath is null, it means that the `path' we get
    ;; is a dir, set it to rpath.
    (if (string= dpath "") (setq dpath rpath))
    ;; calculate relative dir to root
    ;; make string like 2013/03/23 become to ../../../
    (setq npath (split-string dpath "/"))
    (dolist (d npath) (setq nroot (concat nroot "../")))
    ;; remove last / and return
    (s-left (- (string-width nroot) 1) nroot)))

(defun blogit-render-header (url)
  "Render the header on each page."
  (blogit-template-render
   :page_header
   (ht ("TITLE"  (or (blogit-parse-option "TITLE") "Untitled"))
       ("AUTHOR" (or (blogit-parse-option "AUTHOR") user-full-name "Unknown Author"))
       ("GENERATOR" blogit-generator-string)
       ("DESCRIPTION" (or (blogit-parse-option "DESCRIPTION") ""))
       ("KEYWORDS" (or (blogit-parse-option "KEYWORDS") ""))
       ("ROOT" (blogit-path-to-root (file-name-directory url)))
       )))

(defun blogit-render-footer (url)
  "Render the footer on each page."
  (blogit-template-render
   :page_header
   (ht ("ROOT" (blogit-path-to-root (file-name-directory url)))

       )))

(defun blogit-render-post (url)
  "Render full post."
  (blogit-template-render
   :blog_post
   (ht ("HEADER" (blogit-render-header url))
       ("TITLE" (or (blogit-parse-option "TITLE") "Untitled"))
       ("CONTENT" (org-export-as 'html nil nil t nil))
       ("FOOTER" (blogit-render-footer url))
       )))

;; TODO: rewrite this function
(defun blogit-generate-url ()
  ()
  (concat
   (directory-file-name blogit-output-dir) "/"
   (directory-file-name (blogit-generate-dir-string (blogit-parse-option "DATE"))) "/"
   (or (blogit-parse-option "URL")
       (blogit-sanitize-string (file-name-base
                                (buffer-file-name (current-buffer)))))
   ".html"))

;;;###autoload
(defun blogit-insert-template (&optional filename)
  "Insert blogit newpost template."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (insert
     (blogit-template-render
      :newpost
      (ht ("TITLE" (file-name-base (or filename "")))
          ("USER"  (or user-full-name user-login-name ""))
          ("EMAIL" (or user-mail-address ""))
          ("DATE"  (format-time-string blogit-date-format))
          ("URL"   (blogit-sanitize-string filename))
          ("LANGUAGE" (or blogit-default-language "en"))
          )))
    (newline-and-indent)))

;;;###autoload
(defun blogit-new-post (filename)
  "Create a new post in FILENAME."
  (interactive "sTitle for new post: ")
  (find-file (concat
              (file-name-as-directory blogit-source-dir) filename ".org"))
  (blogit-insert-template filename))

;;;###autoload
(defun blogit-publish-current-file ()
  (interactive)
  (let ((out-dir (file-name-directory (blogit-generate-url)))
	(url (blogit-generate-url)))
    ;; Check if #+DATE: option exist, create it if not exist.
    (or (blogit-parse-option "DATE")
	(blogit-modify-option "DATE" (format-time-string blogit-date-format)))
    ;; Update #+LAST_MODIFIED: option according to blogit-date-format.
    (blogit-modify-option "LAST_MODIFIED" (format-time-string blogit-date-format))
    ;; create dir for output files
    (mkdir out-dir t)
    ;; generate file
    (blogit-string-to-file (blogit-render-post url) url)))

(provide 'blogit)

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;;


;; for test

(setq blogit-source-dir ".")
(setq blogit-output-dir ".")
