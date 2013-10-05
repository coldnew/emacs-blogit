;;; blogit.el ---

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html blog org-mode
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
(require 'dired-sync nil t)

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
  "Template directory, this dir must located under
`blogit-src-dir' and will not be copy to
blogit-output-dir after publish."
  :group 'blogit :type 'string)

(defcustom blogit-style-dir "style/"
  "Stylw directory, this dir will must located under
`blogit-src-dir' and will be cpoied to
blogit-output-dir after publish."
  :group 'blogit :type 'string)

(defcustom blogit-always-copy-theme-dir t
  "If t, always copy blogit-style-dir to blogit-output-dir
when use `blogit-publish-blog', else only do this when
use `blogit-republish-blog'."
  :group 'blogit :type 'boolean)

(setq blogit-project-list
      `("blogit"
        :base-directory ,blogit-source-dir
        :publishing-directory ,blogit-output-dir
        :base-extension "org"
        :publishing-function org-blogit-publish-to-html
        :recursive t
        ))

(defcustom blogit-default-type 'blog
  "Configure default blogit page type. Currently we only support two type."
  :group 'blogit
  :type '(choice
          (const :tag "blog" blog)
          (const :tag "static" static)))

(defcustom blogit-date-format "%Y/%02m/%02d %02H:%02M:%02S"
  "Format for printing a date in the sitemap.
See `format-time-string' for allowed formatters."
  :group 'blogit :type 'string)

(defvar blogit-output-format-list
  '(:blog-type   blog
                 :blog-dir   "blog/%y/%m/%d"
                 :static-type static
                 :static-dir  ""
                 )
  "Output dir formate for blogit, use blogit-output-dir as root when
this value is empty string.

The dir will be format by `format-time-string', but the time is according to
your #+DATE info.

Currently blogit only support following format:

    %y : year   eq: 2013
    %m : month  eq: 03
    %d : day    eq: 23")

(defvar blogit-template-list
  '((:page_header      . "page_header.html")
    (:page_footer      . "page_footer.html")
    (:plugin_analytics . "plugin_analytics.html")
    (:plugin_disqus    . "plugin_disqus.html")

    ;; FIXME: still not use
    (:page_navigator   . "page_navigator.html")
    (:newpost          . "newpost.org")
    (:blog_rss         . "blog_rss.html")
    (:blog_post        . "blog_post.html")
    (:blog_index       . "blog_index.html")
    (:index            . "index.html")
    )
  "Template filename define for blogit to parse.")


;;; Internal variables

(defvar blogit-linked-file-cache nil
  "Cache file to store which file will be copied to output dir.")

(defvar blogit-ignore-dir
  `(,blogit-template-dir ,blogit-style-dir)
  "When publish, ignore these dir under `blogit-source-dir'.")


;;; Internal functions

(defun blogit--get-post-type (info)
  "Get current post type, return `blogit-default-type' if not found."
  (let* ((typestr (blogit--parse-option info :type))
         (key (intern (format ":%s-type" typestr)))
         (type (plist-get blogit-output-format-list key)))
    (cond
     ((eq type 'blog) 'blog)
     ((eq type 'static) 'static)
     (t blogit-default-type))))

(defun blogit--get-post-dir-format (info)
  "Get post output format according to post type."
  (let* ((type (blogit--get-post-type info))
         (key (intern (format ":%s-dir" (symbol-name type)))))

    (plist-get blogit-output-format-list key)))

(defun blogit--get-post-filename (info &optional filename)
  "Get current post export filename."
  (let ((name (blogit--parse-option info :url)))
    (if name name
      (blogit--sanitize-string (or filename
                                   (file-name-base (buffer-file-name (buffer-base-buffer))))))))

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
(defun blogit--template-fullfile (key)
  "Get match template filename with fullpath according to key."
  (convert-standard-filename
   (concat blogit-source-dir "/" blogit-template-dir
           (cdr (assoc key blogit-template-list)))))

(defun blogit--sanitize-string (s)
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

(defun blogit--parse-date-string1 (date-str yn mn dn)
  "Helper function to return date list for `blogit--parse-date-string'."
  (list
   :year (format "%04d" (string-to-int (match-string yn date-str)))
   :month (format "%02d" (string-to-int (match-string mn date-str)))
   :day (format "%02d" (string-to-int (match-string dn date-str)))))

;; FIXME: this function may be has some error when date-string is nil
(defun blogit--parse-date-string (date-string)
  "Returns yyyy/mm/dd format of date-string
For examples:
   [Nov. 28, 1994]     => [1994/11/28]
   [November 28, 1994] => [1994/11/28]
   [11/28/1994]        => [1994/11/28]
Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.

This function is used to create directory for new blog post.
"
  (if (not (stringp date-string)) ""
    (let ((date-str date-string)
          date-list year month date yyyy mm dd)
      (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str))
      (cond
       ;; USA convention of mm/dd/yyyy
       ((string-match
         "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9][0-9][0-9][0-9]\\)" date-str)
        (blogit--parse-date-string1 date-str 3 1 2))

       ;; yyyy/mm/dd
       ((string-match
         "^\\([0-9][0-9][0-9][0-9]\\)/\\([0-9]+\\)/\\([0-9]+\\)" date-str)
        (blogit--parse-date-string1 date-str 1 2 3))

       ;; some ISO 8601. yyyy-mm-dd
       ((string-match
         "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9]+\\)-\\([0-9]+\\)" date-str)
        (blogit--parse-date-string1 date-str 1 2 3))

       (t (progn
            (setq date-list (parse-time-string date-str))
            (setq year (nth 5 date-list))
            (setq month (nth 4 date-list))
            (setq date (nth 3 date-list))
            (setq yyyy (number-to-string year))
            (setq mm (if month (format "%02d" month) ""))
            (setq dd (if date (format "%02d" date) ""))
            (blogit--parse-date-string1 (concat yyyy "/" mm "/" dd) 1 2 3)))))))

(defun blogit--build-export-dir (info)
  "Build export dir path according to #+DATE: option."
  (let* ((date-str  (blogit--parse-option info :date))
         (date-list (blogit--parse-date-string date-str))
         (dir-format (blogit--get-post-dir-format info))
         (dir-1 (split-string dir-format "/"))
         (dir ""))
    (dolist (d dir-1)
      (cond
       ((string= d "%y") (setq dir (concat dir (plist-get date-list :year))))
       ((string= d "%m") (setq dir (concat dir (plist-get date-list :month))))
       ((string= d "%d") (setq dir (concat dir (plist-get date-list :day))))
       (t (setq dir (concat dir d))))
      (setq dir (concat dir "/")))
    (format "%s/%s" (directory-file-name blogit-output-dir) (replace-regexp-in-string "//*" "/" dir))))

;; FIXME: what about ./ ?
(defun blogit--path-to-root (path)
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

(defun blogit--calculate-post-relative-path (path)
  "Calculate post path from root."
  (let* ((epath (expand-file-name (blogit--get-post-url path)))
         (filename (file-name-nondirectory epath))
         (path-dir (file-name-directory epath))
         (rpath (s-replace (expand-file-name blogit-output-dir) "" path-dir)))
    (replace-regexp-in-string "//*" "/"
                              (concat (blogit--path-to-root  path-dir) "/"  rpath filename))))

(defun blogit--render-template (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (blogit--template-fullfile type) type)))
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

(defun blogit--modify-option (option value)
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

(defun blogit--check-post-file (file)
  "If file is valid blogit post, return t, else nil."
  (if (file-directory-p file) nil
    (with-temp-buffer
      (insert-file-contents file)
      ;; all blogit valid post must contains #+DATE option.
      (if (blogit--parse-option nil :date) t nil))))

(defun blogit--do-copy (src dst &optional copyf args)
  "Copy SRC into DST. If `dired-do-sync' is found it would be
preferred. Otherwise, `copy-directory' or `copy-files' would be
used.

A copy function COPYF and its arguments ARGS could be specified."
  (let* ((dirp (file-directory-p src))
         (dst-dir (file-name-directory dst))
         (copyf (cond
                 (copyf copyf)
                 ((functionp 'dired-do-sync) 'dired-do-sync)
                 (dirp 'copy-directory)
                 (t 'copy-file)))
         (args (or args
                   (when (eq 'copy-file copyf) '(t t t)))))

    (unless (or (not dst-dir) (file-exists-p dst-dir)) (make-directory dst-dir t))

    (when (file-exists-p src)
      (apply copyf src dst args))))

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
    ("TITLE"  (or (blogit--parse-option info :title) "Untitled"))
    ("AUTHOR" (or (blogit--parse-option info :author) user-full-name "Unknown"))
    ("EMAIL" (or (blogit--parse-option info :email) user-mail-address ""))
    ("DATE" (or (blogit--parse-option info :date) ""))
    ("URL" (or (blogit--parse-option info :url) ""))
    ("LANGUAGE" (or (blogit--parse-option info :language) "en"))
    ("DESCRIPTION" (or (blogit--parse-option info :description) ""))
    ("KEYWORDS" (or (blogit--parse-option info :keywords) ""))
    ("DISQUS" (or (blogit--render-disqus-template info) ""))
    ("ANALYTICS" (or (blogit--render-analytics-template info) ""))
    ("ROOT" (blogit--path-to-root (blogit--build-export-dir info)))
    ,@pairs))


;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit 'html
  :options-alist
  '(
    (:analytics         "ANALYTICS"         nil   nil   t)
    (:disqus            "DISQUS"            nil   nil   t)
    (:url               "URL"               nil   nil   t)
    (:type              "TYPE"              nil   nil   t)

    ;; disable org-html default style
    ;; WARNING: DO NOT edit folling since it may break blogit functions
    (:html-head-include-default-style nil "html-style" nil)
    (:html-head-include-scripts nil "html-scripts" nil)
    )

  :translate-alist
  '((link . org-blogit-html-link)
    (template     . org-blogit-template)))

(defun blogit--render-header-template (info)
  (blogit--render-template :page_header (blogit--build-context info)))

(defun blogit--render-footer-template (info)
  (blogit--render-template :page_footer (blogit--build-context info)))

(defun blogit--render-disqus-template (info)
  (when (or (blogit--parse-option info :disqus) blogit-disqus-shortname)
    (blogit--render-template
     :plugin_disqus
     (ht ("DISQUS" (or (blogit--parse-option info :disqus) blogit-disqus-shortname))))))

(defun blogit--render-analytics-template (info)
  (when (or (blogit--parse-option info :analytics) blogit-google-analytics-id)
    (blogit--render-template
     :plugin_analytics
     (ht ("ANALYTICS" (or (blogit--parse-option info :analytics) blogit-google-analytics-id))))))


;; FIXME:
(defun blogit--check-post-file (file)
  "If file is valid blogit post, return t, else nil."
  (if (and (file-directory-p file) (file-exists-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      ;; all blogit valid post must contains #+DATE option.
      (if (blogit--parse-option nil :date) t nil))))

(defun blogit--get-post-url (file)
  "Get the post url from file."
  (if (and (file-directory-p file) (file-exists-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      ;; all blogit valid post must contains #+DATE option.
      (format "%s%s.html" (blogit--build-export-dir nil)
              (blogit--get-post-filename nil file)))))

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
        (if (blogit--check-post-file encode-path)
            ;; if file is really blogit post, get it url
            (setq new-path (blogit--calculate-post-relative-path encode-path))
          (setq file-to-cache raw-path)))
       (t (setq file-to-cache raw-path)))
      ;; add file to cache, we will use this cache to copy file
      (when file-to-cache
        (add-to-list 'blogit-linked-file-cache file-to-cache))

      (if (not new-path)
          (setq new-path (concat file-dir "/"
                                 (file-name-nondirectory encode-path))))

      (when new-path
        ;; remove `file://' prefix from html-link when raw-path is
        ;; absolute path
        (if (file-name-absolute-p raw-path)
            (setq html-link (s-replace (concat link-prefix "file://") link-prefix html-link)))

        ;; Since some of raw-path use absolute dir, some use relative
        ;; dir (like image), we make all raw-path to use relative path
        ;; here if it is at the same dir as post.
        (setq raw-path (s-replace (file-name-directory (buffer-file-name)) "" raw-path))

        ;; we also need to modify org-html-link to relative path
        ;; for our post
        (setq html-link (s-replace (concat "=\"" raw-path) (concat "=\"" new-path) html-link))))
    ;; done and done, now return our new-link
    html-link))

(defun org-blogit-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((meta-info (org-html--build-meta-info info))
         (preamble-info (org-html--build-pre/postamble 'preamble info))
         ;;(postamble-info (org-html--build-pre/postamble 'postamble info))
         ;; FIXME: discard postamble info, this method is not elegant
         (postamble-info ""))
    ;; we override some of function in org-html-template to make it
    ;; more easy to build our template
    (flet ((org-html--build-meta-info (info)
                                      (concat
                                       meta-info
                                       (blogit--render-header-template info)))
           (org-html--build-pre/postamble (type info)
                                          (cond
                                           ((eq type 'preamble) preamble-info)
                                           ((eq type 'postamble)
                                            (concat postamble-info
                                                    (blogit--render-footer-template info))))))

      (org-html-template contents info))))

(defun blogit-export-linked-file (output-file)
  "Copy all linked file to dst."
  (let ((cache blogit-linked-file-cache)
        (pub-dir (concat (file-name-directory output-file) (file-name-base output-file))))

    ;; copy file to blogit-output-dir if file in cache.
    (dolist (src cache)
      (let ((dst (concat pub-dir "/" (file-name-nondirectory src))))
        (blogit--do-copy src dst)))
    ;; do not forget to clear cache
    (setq blogit-linked-file-cache nil)))


;;; Rewrite some org function to make blogit work more properly

(defun blogit-export-output-file-name (extension &optional subtreep pub-dir)
  "Return output file's name according to buffer specifications.

EXTENSION is a string representing the output file extension,
with the leading dot.

With a non-nil optional argument SUBTREEP, try to determine
output file's name by looking for \"EXPORT_FILE_NAME\" property
of subtree at point.

When optional argument PUB-DIR is set, use it as the publishing
directory.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return file name as a string.

This function is rewrite from `org-export-output-file-name'."
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
         (pub-dir (blogit--build-export-dir nil))
         (base-name
          ;; File name may come from EXPORT_FILE_NAME subtree
          ;; property, assuming point is at beginning of said
          ;; sub-tree.
          (file-name-sans-extension
           (or (and subtreep
                    (org-entry-get
                     (save-excursion
                       (ignore-errors (org-back-to-heading) (point)))
                     "EXPORT_FILE_NAME" t))
               ;; File name may be extracted from buffer's associated
               ;; file, if any.
               (and visited-file (file-name-nondirectory visited-file))
               ;; Can't determine file name on our own: Ask user.
               (let ((read-file-name-function
                      (and org-completion-use-ido 'ido-read-file-name)))
                 (read-file-name
                  "Output file: " pub-dir nil nil nil
                  (lambda (name)
                    (string= (file-name-extension name t) extension)))))))
         (output-file
          ;; Build file name.  Enforce EXTENSION over whatever user
          ;; may have come up with.  PUB-DIR, if defined, always has
          ;; precedence over any provided path.
          (concat (file-name-as-directory (blogit--build-export-dir nil))
                  (file-name-nondirectory (blogit--get-post-filename nil base-name))
                  extension)))

    ;; If output dir does not exist, create it
    (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))

    ;; If writing to OUTPUT-FILE would overwrite original file, append
    ;; EXTENSION another time to final name.
    (if (and visited-file (org-file-equal-p visited-file output-file))
        (concat output-file extension)
      output-file)))

(defun blogit-publish-org-to (backend filename extension plist &optional pub-dir)
  "Publish an Org file to a specified back-end.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.

Optional argument PUB-DIR, when non-nil is the publishing
directory.

Return output file name.

This function is rewrite from `org-publish-org-to'."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename))))
    (prog1 (with-current-buffer work-buffer
             (let ((output-file
                    (blogit-export-output-file-name extension nil pub-dir))
                   (body-p (plist-get plist :body-only)))

               (org-export-to-file backend output-file
                 nil nil nil body-p
                 ;; Add `org-publish-collect-numbering' and
                 ;; `org-publish-collect-index' to final output
                 ;; filters.  The latter isn't dependent on
                 ;; `:makeindex', since we want to keep it up-to-date
                 ;; in cache anyway.
                 (org-combine-plists
                  plist
                  `(:filter-final-output
                    ,(cons 'org-publish-collect-numbering
                           (cons 'org-publish-collect-index
                                 (plist-get plist :filter-final-output))))))
               ;; copy all needed file for output-file
               (blogit-export-linked-file output-file)))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))


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
  "Publish an org file to HTML for blogit.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((do-publish t)
        (file-dir (file-name-directory (expand-file-name filename))))

    ;; check if file is not under blogit-ignore-dir
    (dolist (d blogit-ignore-dir)
      (if do-publish
          (if (string= file-dir
                       (file-name-directory (expand-file-name (concat blogit-source-dir "/" d))))
              (setq do-publish nil))))

    ;; do not publish if file is under blogit template dir
    (when do-publish
      (blogit-publish-org-to 'blogit filename
                             (concat "." (or (plist-get plist :html-extension)
                                             org-html-extension "html"))
                             plist pub-dir))))

;;;###autoload
(defun blogit-publish-blog ()
  (interactive)
  (let* ((org-publish-timestamp-directory
          (convert-standard-filename (concat blogit-cache-dir "/")))
         (org-publish-cache nil)
         (source-style-dir (convert-standard-filename (concat blogit-source-dir "/" blogit-style-dir)))
         (output-dir (convert-standard-filename (concat blogit-output-dir "/")))
         (output-style-dir (concat output-dir blogit-style-dir))
         (copy-theme-dir blogit-always-copy-theme-dir))
    (org-publish-project blogit-project-list)
    ;; Copy theme-dir to blogit-output-dir when publish
    ;; if theme dir does not exit, re-copy again
    (unless (file-exists-p output-style-dir) (setq copy-theme-dir t))
    (when copy-theme-dir
      (blogit--do-copy source-style-dir output-dir))))

;;;###autoload
(defun blogit-republish-blog ()
  (interactive)
  (let* ((org-publish-timestamp-directory
          (convert-standard-filename (concat blogit-cache-dir "/")))
         (source-style-dir (convert-standard-filename (concat blogit-source-dir "/" blogit-style-dir)))
         (output-dir (convert-standard-filename (concat blogit-output-dir "/")))
         (org-publish-cache nil))
    (if (file-exists-p org-publish-timestamp-directory)
        (delete-directory org-publish-timestamp-directory t nil))
    (org-publish-project blogit-project-list)
    ;; copy theme to blogit-output-dir
    (blogit--do-copy source-style-dir output-dir)))

(provide 'blogit)
;;; blogit.el ends here.
