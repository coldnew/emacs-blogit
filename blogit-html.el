;;; blogit-html.el --- HTML Back-End for Blogit Export Engine

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html blog org-mode
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.2

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

;; blogit dependencies
(require 'blogit-core)
(require 'blogit-vars)
(require 'blogit-bootstrap)

(eval-when-compile (require 'cl))


;;; Core function for building blogit HTML

(defmacro blogit--build-context-html (info &rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)

This function is used to create context for blogit-render function,
many useful context is predefined here, but you can overwrite it.
"
  `(blogit--build-context
    ,info

    ;; context derived from ox-html
    ("HTML_META" (if ,info (org-html--build-meta-info ,info)))
    ("HTML_HEAD" (if ,info (org-html--build-head ,info)))
    ("HTML_MATHJAX" (if ,info (org-html--build-mathjax-config ,info)))
    ("HTML_PREAMBLE" (if ,info (org-html--build-pre/postamble 'preamble ,info)))
    ("HTML_POSTAMBLE" (if ,info (org-html--build-pre/postamble 'postamble ,info)))

    ;; context from blogit template
    ("PAGE_HEADER" (blogit--render-header-template ,info))
    ("PAGE_NAVIGATOR" (blogit--render-navigator-template ,info))
    ("PAGE_FOOTER" (blogit--render-footer-template ,info))

    ,@pairs))


;;; Template render function

(defun blogit--render-header-template (info)
  (blogit--render-template :page_header (blogit--build-context info)))

(defun blogit--render-navigator-template (info)
  (blogit--render-template :page_navigator (blogit--build-context info)))

(defun blogit--render-footer-template (info)
  (blogit--render-template :page_footer (blogit--build-context info)))




;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit-html 'html
  :options-alist
  '(
    (:analytics         "ANALYTICS"         nil   nil   nil)
    (:disqus            "DISQUS"            nil   nil   nil)
    (:url               "URL"               nil   nil   nil)
    (:blogit_type       "BLOGIT_TYPE"       nil   nil   nil)

    ;; FIXME: maybe this can remove ?
    ;; disable org-html default style
    ;; WARNING: DO NOT edit folling since it may break blogit functions
    ;;(:html-head-include-default-style nil "html-style" nil)
    ;;(:html-head-include-scripts nil "html-scripts" nil)
    )

  :translate-alist
  '((keyword . org-blogit-html-keyword)
    (link . org-blogit-html-link)
    (template     . org-blogit-template)
    (special-block . org-blogit-special-block)))

(defun blogit--check-post-file (file)
  "If file is valid blogit post, return t, else nil."
  (if (and (file-directory-p file) (file-exists-p file))
      nil
    (blogit--file-in-temp-buffer
     file
     ;; BLogit use following way to check if the post is valid
     ;;
     ;; 1. All blogit valid post must contains `DATE' option.
     ;; 2. If post type is `draft', it's not valid
     ;; 3. All blogit post must under `(blogit-project-info :base-directory)'.

     ;; FIXME: This algorithm may porduce some problem ?
     (if (and (blogit--parse-option nil :date)
              (not (eq 'draft (blogit--get-post-type nil)))
              (s-starts-with?
               (directory-file-name (expand-file-name (concat (blogit-project-info :base-directory) "/")))
               (file-name-directory (expand-file-name file))))

         t nil))))

(defun blogit--get-post-url (file)
  "Get the post url from file."
  (if (and (file-directory-p file) (file-exists-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      ;; all blogit valid post must contains #+DATE option.
      (format "%s%s" (blogit--build-export-dir nil)
              (blogit--get-post-filename nil file)))))

;;;; Special Block

(defun org-blogit-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((block-type (upcase
		      (org-element-property :type special-block))))
    (cond
     ((string= block-type "BLOGIT") (blogit--html-special-block special-block contents info))
     (t (org-html-special-block special-block contents info)))))

(defun blogit--html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (downcase
		      (org-element-property :type special-block)))
	 (contents (or contents ""))
	 (html5-fancy (and (org-html-html5-p info)
			   (plist-get info :html-html5-fancy)
			   (member block-type org-html-html5-elements)))
	 (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
	(setq attributes (plist-put attributes :class
				    (if class (concat class " " block-type)
				      block-type)))))
    (setq attributes (org-html--make-attribute-string attributes))
    (when (not (equal attributes ""))
      (setq attributes (concat " " attributes)))
    (if html5-fancy
	(format "<%s%s>\n%s</%s>" block-type attributes
		contents block-type)
      (format "<div%s>\n%s\n</div>" attributes contents))))

;;;; Keyword

(defun org-blogit-html-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (upcase (org-element-property :key keyword)))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "BLOGIT_SOURCE") (blogit-bootstrap-publish-source info value))
     (t (org-html-keyword keyword contents info)))))

(defun org-blogit-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'.

In this function, we also add link file"
  (let* ((org-html-link-org-files-as-html nil)
         (type (org-element-property :type link))
         (raw-path (expand-file-name (org-element-property :path link)))
         (encode-path (expand-file-name (org-link-unescape raw-path)))
         (html-link (org-html-link link desc info))
         (file-dir (file-name-base (blogit--get-post-filename info)))
         (link-prefix "<a href=\"")
         new-path file-to-cache)
    ;; file
    (when (string= type "file")
      (cond
       ((string= ".org"
                 (downcase (file-name-extension encode-path ".")))
        ;; check if the file is also a blogit post, if t, not add
        ;; file to cache.
        ;; FIXME: How if I just want to lonk a org file, and it also
        ;; is a blogit post ?
        (if (blogit--check-post-file encode-path)
            ;; if file is really blogit post, get it url
            (setq new-path (blogit--calculate-post-relative-path encode-path))
          (setq file-to-cache raw-path)))
       (t (setq file-to-cache raw-path)))
      ;; add file to cache, we will use this cache to copy file
      (when file-to-cache
        (add-to-list 'blogit-linked-cache file-to-cache))

      (if (not new-path)
          (setq new-path (concat file-dir "/"
                                 (file-name-nondirectory encode-path))))

      (when new-path
        ;; remove `file://' prefix from html-link when raw-path is
        ;; absolute path
        (if (file-name-absolute-p raw-path)
            (setq html-link (s-replace (concat link-prefix "file://") link-prefix html-link)))

	;; Convert raw-path to it's true name file
	(setq raw-path (file-truename raw-path))

        ;; Since some of raw-path use absolute dir, some use relative
        ;; dir (like image), we make all raw-path to use relative path
        ;; here if it is at the same dir as post.
        (setq raw-path (s-replace (file-name-directory
				   (file-truename (or blogit-current-file (buffer-file-name)))) "" raw-path))

        ;; we also need to modify org-html-link to relative path
        ;; for our post
        (setq html-link (s-replace (concat "=\"" raw-path) (concat "=\"" new-path) html-link))))
    ;; done and done, now return our new-link
    html-link))

(defun org-blogit-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((tags (blogit--get-tags info))
         (tags-sanitize (mapcar 'blogit--sanitize-string tags))
         (tags-url (mapcar '(lambda (x)
                              (blogit--remove-dulpicate-backslash
                               (concat
                                (blogit-project-info :blog-url) "/"
                                (blogit-project-info :tags-directory-name) "/" x ".html")))
                           tags-sanitize))
         (tags-list ((lambda (&rest args)
                       (apply (function mapcar*) (function list) args))
                     tags-url tags)))

    (blogit--render-template
     ;; Render HTML according to blogit type and suitable template
     (blogit--get-post-template info)
     ;; Build HTML context
     (blogit--build-context-html
      info
      ;; CONTENT always mean the file content we care
      ("CONTENT" (org-export-as 'blogit-html nil nil t nil))
      ;; tag list
      ("TAG_LIST" (--map (ht ("TAG_URL" (car it)) ("TAG_NAME" (cadr it))) tags-list))
      ))))

(defun blogit--export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."

  (let ((outbuf (org-export-to-buffer
                    'blogit-html "*Blogit HTML Export*"
                  subtreep visible-only body-only ext-plist))
        (org-export-coding-system org-html-coding-system))
    ;; Set major mode.
    (with-current-buffer outbuf (set-auto-mode t))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf)
      ;; Indent html buffer to make debug more easy
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max))
      (untabify (point-min) (point-max)))))


;;; End-user functions

;;;###autoload
(defun blogit-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)

  ;; Find blogit project info to initialize, if file does not contains
  ;; in any blogit-project-alist, ask user to select template to
  ;; render.
  (if (blogit--swtich-to-file-project (buffer-file-name))
      (blogit--export-as-html)
    (blogit--select-project
     'blogit--export-as-html
     "No match project found. Select Project to render current file.")))

(provide 'blogit-html)
;;; blogit-html.el ends here.
