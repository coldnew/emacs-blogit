;;; blogit.el ---

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
(require 'dired-sync nil t)

(eval-when-compile (require 'cl))


;;; Blogit info

(defconst blogit-version "0.2"
  "Blogit version string.")

(defconst blogit-url
  "http://github.com/coldnew/emacs-blogit"
  "Url for blogit.")


;;; User Configuration Variables

(setq blogit-default-project-list
      '("Blogit"

        ;; same options as `org-publist-project-alist'
        :base-directory ""
        :publishing-directory ""
        :recursive t
        :base-extension "org"

	;; extra options defined in blogit
        :default-language "en"
        :template-directory-name "templates"
        :style-directory-name    "style"

        :copy-style-directory-method 'always

        :google-analytics ""
        :disqus    ""

        :blog-url   ""
        :blog-title ""
        :blog-description ""

        :type-list blogit-type-list
        :template-list blogit-template-list
        :publishing-function org-blogit-publish-to-html

        :export-rss t 			; TODO:
        :export-rss-number 10

        :export-recents-post t		; TODO:
        :export-recents-number 10

        ;; Advanced options for customize blogit
	;; some will be set by `blogit-initialize-project'
        :blogit-sanitize-length 5
        :blogit-date-format "%Y-%02m-%02d %02H:%02M:%02S"
        :blogit-ignore-directory-list nil

        :blogit-cache-directory ""
        :blogit-cache-directory-name ".cache"
	:blogit-cache-file ""

	:blogit-style-directory ""
	:blogit-template-directory ""
	:blogit-default-type blog
	:blogit-tags-directory-name "tags"
        ))

(defvar blogit-project-list nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

1. A well-formed property list with an even number of elements,
   alternating keys and values, specifying parameters for the
   publishing process.

     \(:property value :property value ... )

2. A meta-project definition, specifying of a list of
   sub-projects:

     \(:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after `:components'
are taken to be components of the project, which group together
files requiring different publishing options.  When you publish
such a project with \\[org-publish], the components all publish.

When a property is given a value in `org-publish-project-alist',
its setting overrides the value of the corresponding user
variable (if any) during publishing.  However, options set within
a file override everything.

Most properties are optional, but some should always be set:

  `:base-directory'

    Directory containing publishing source files.

  `:publishing-directory'

    Directory (possibly remote) where output files will be
    published.
"
  )

;; FIXME: not let anyone modified this ?
(defvar blogit-type-list
  (list
   :draft  '(:type draft)
   :blog   '(:type blog   :root "blog" :filepath "%y/%m"  :filename "%d_%s.html")
   :static '(:type static :root ""     :filepath ""       :filename "%s.html"))
  "Output dir formate for blogit, use `(blogit-project-info :publishing-directory)' as root when
this value is empty string.

The dir will be format by `format-time-string', but the time is according to
your #+DATE info.

Currently blogit only support following format:

    %y : year   eq: 2013
    %m : month  eq: 03
    %d : day    eq: 23
    %S : filename without any sanitize. `NOTE: Not suggest use this.'
    %s : filename sanitize by `blogit--sanitize-string', if `#+URL' is specified, use it.
         When `#+URL' contains backslash, this fotmat will be ignore.
")

(defvar blogit-template-list
  (list
   :page_header        "page_header.html"
   :page_footer        "page_footer.html"
   :plugin_analytics   "plugin_analytics.html"
   :plugin_disqus      "plugin_disqus.html"
   :rss                "rss.xml"
   :newpost            "newpost.org"

   ;; FIXME: still not use
   :page_navigator    "page_navigator.html"
   :blog_post         "blog_post.html"
   :blog_index        "blog_index.html"
   :index             "index.html"
   )
  "Template filename define for blogit to parse.")


;;; Internal variables

(defvar blogit-cache nil
  "Cache to store post info, this cache will be used to
generate rss and tage.")

(defvar blogit-linked-cache nil
  "Cache to store which file will be copied to output dir.")

(defvar blogit-current-project nil
  "Cache to store current project plist info.")


;;; Internal functions

(defun blogit--string-to-key (string)
  "Conver string to key. eq: \"test\" -> :test"
  (intern (format ":%s" string)))

(defun blogit--symbol-to-key (symbol)
  "Conver symbol to key. eq: test -> :test"
  (blogit--string-to-key (symbol-name symbol)))

(defun blogit--key-to-string (key)
  "Conver key to string. eq: :test -> \"test\""
  (let ((key-str (symbol-name key)))
    (s-right (- (length key-str) 1) key-str)))

(defun blogit--key-to-symbol (key)
  "Conver key to symbol. eq: test -> :test"
  (intern (blogit--key-to-string key)))

(defun blogit--remove-dulpicate-backslash (str)
  "Remove dulpicate backslash for str."
  (replace-regexp-in-string "//*" "/"  str))


;;; Internal Project Control functions

(defun blogit-combine-project (project)
  (let ((project-name (car project))
        (project-plist (cdr project)))
     (cons project-name
           (org-combine-plists
            (cdr blogit-default-project-list) project-plist))))

(defun blogit-project-info (key)
  "Return project info according to key."
  (if (eq key :project)
      (car blogit-current-project)
    (plist-get (cdr blogit-current-project) key)))

(defun blogit--plist-remove (plist &rest keys)
  (let ((proper-plist plist))
    ;; FIXME: possible optimization: (plist-remove '(:x 0 :a 1 :b 2) :a)
    ;; could return the tail without consing up a new list.
    (loop for (key . rest) on plist by #'cddr
	  unless (memq key keys)
	  collect key and collect (first rest))))

(defun blogit-project-set (key val)
  "Setting blogit-current-project value according to key."
  (let* ((project-name (car blogit-current-project))
	(project-list (cdr blogit-current-project))
	(new-list (blogit--plist-remove project-list key)))
    (push val new-list)
    (push key new-list)
    (setq blogit-current-project (cons project-name new-list))))

(defun blogit-project-convert-standard-filename (key)
  "Convert directory path to standard."
  (let ((dir (blogit-project-info key)))
    (blogit-project-set
     key
     (convert-standard-filename
      (blogit--remove-dulpicate-backslash
       (concat dir "/"))))))

(defun blogit-initialize-project (project-list)
  "Initial some project value for current project."

  ;; Add porject-list to current-project
  (setq blogit-current-project
        (blogit-combine-project blogit-project-list))

  ;; Initial ignore dir
  ;; FIXME: How to let anyone add more ignore dir?
  (blogit-project-set
   :blogit-ignore-directory-list
   (list
    (blogit-project-info :template-directory-name)
    (blogit-project-info :style-directory-name)))

  ;; Initial cache dir
  (blogit-project-set
   :blogit-cache-directory
   (concat (blogit-project-info :publishing-directory) "/"
	   (blogit-project-info :blogit-cache-directory-name) "/"))

  ;; Initial cache file
  (blogit-project-set
   :blogit-cache-file
   (concat (blogit-project-info :blogit-cache-directory)
	   "/"
	   (format "%s-publish.cache" (car blogit-current-project))))

  (blogit-project-set
   :blogit-style-directory
   (concat (blogit-project-info :base-directory) "/"
	   (blogit-project-info :style-directory-name) "/"))

  (blogit-project-set
   :blogit-template-directory
   (concat (blogit-project-info :base-directory) "/"
	   (blogit-project-info :template-directory-name) "/"))

  ;; Convert standard name for project directory path
  (blogit-project-convert-standard-filename :base-directory)
  (blogit-project-convert-standard-filename :publishing-directory)
  (blogit-project-convert-standard-filename :blogit-cache-directory)
  (blogit-project-convert-standard-filename :blogit-style-directory)
  (blogit-project-convert-standard-filename :blogit-template-directory)
  )


;;; Internal functions

(defmacro blogit--file-in-temp-buffer (filename &rest pairs)
  "Helper macro to open file in temp buffer, and execute blogit function."
  `(when (file-exists-p ,filename)
     (with-temp-buffer (insert-file-contents ,filename) ,@pairs)))

(defun blogit--get-post-type (info &optional filename)
  "Get current post type, return `(blogit-project-info :blogit-default-type)' if not found.
When filename is specified, open the file and get it's post type."
  (if filename
      (blogit--file-in-temp-buffer filename (blogit--get-post-type nil))
    (let* ((typestr (blogit--parse-option info :type))
           (key (blogit--string-to-key typestr))
           (info (plist-get blogit-type-list key))
           (type (plist-get info :type)))
      (cond
       ((eq type 'blog) 'blog)
       ((eq type 'static) 'static)
       ((eq type 'draft)  'draft)
       (t (blogit-project-info :blogit-default-type))))))

;; FIXME: This function looks ogly
(defun blogit--format-to-s-format (str)
  "Unelegant way to convert blogit file fotmat to s-format."
  (replace-regexp-in-string
   "%y" "${year}"
   (replace-regexp-in-string
    "%m" "${month}"
    (replace-regexp-in-string
     "%d" "${day}"
     (replace-regexp-in-string
      "%S" "${filename}"
      (replace-regexp-in-string "%s" "${sanitize}" str))))))

(defun blogit--build-format-list (info &optional file)
  "Cons cells for blogit s-formate."
  (let* ((date-str  (blogit--parse-option info :date))
         (date-list (blogit--parse-date-string date-str))
         (year (or (plist-get date-list :year) ""))
         (month (or (plist-get date-list :month) ""))
         (day (or (plist-get date-list :day) ""))
         (url (or (blogit--parse-option info :url) ""))
         (filename (file-name-base (or file (buffer-base-buffer) "")))
         (sanitize (if (not (string= "" url)) url
                     (s-left (blogit-project-info :blogit-sanitize-length) (blogit--sanitize-string filename)))))
    (list
     (cons "year" year)
     (cons "month" month)
     (cons "day" day)
     (cons "filename" filename)
     (cons "sanitize" sanitize))))

(defun blogit--get-filepath-format (info)
  "Get post output dir format according to post type.
Use \"\" as fallback."
  (let* ((type (blogit--get-post-type info))
         (info (plist-get blogit-type-list (blogit--symbol-to-key type))))
    (blogit--remove-dulpicate-backslash
     (concat
      (or (plist-get info :root) "") "/"
      (or (plist-get info :filepath) "")))))

(defun blogit--get-filename-format (info)
  "Get post output filename format according to post type.
Use \"%s\" as fallback."
  (let* ((type (blogit--get-post-type info))
         (info (plist-get blogit-type-list (blogit--symbol-to-key type))))

    (or (plist-get info :filename) "%s")))

(defun blogit--get-post-filename (info &optional filename)
  "Get current post export filename. When #+URL contains `backslash',
the default format will be ignored."
  (let* ((url (blogit--parse-option info :url))
         (use-url-p (s-contains? "/" (or url ""))))
    (if use-url-p url
      (let* ((blogit-format (blogit--get-filename-format info))
             ;; rebuild the formate and use s-format to formate it
             (filename-format (blogit--format-to-s-format blogit-format)))
        (s-format filename-format 'aget (blogit--build-format-list info filename))))))

(defun blogit--get-tags (info &optional filename)
  "Get blogit tags in list from info or filename."
  (let* ((tag-opt (or (blogit--parse-option info :tags filename) ""))
         (tags (split-string tag-opt " ")))
    ;; remove empty string and dulpicate tag
    (remove-duplicates (remove "" tags) :test 'string=)))

;; FIXME:
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

;; FIXME: how about remove this ?
(defun blogit--template-to-string (file)
  "Read the content of FILE in template dir and return it as string."
  (blogit--file-to-string file))

(defun blogit--template-fullfile (key)
  "Get match template filename with fullpath according to key."
  (convert-standard-filename
   (concat (blogit-project-info :base-directory) "/"
           (blogit-project-info :template-directory-name) "/"
           (plist-get blogit-template-list key))))

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
        collect (s-left 1 (sha1 (char-to-string (if cd (car cd) c))))
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
  (let* ((filepath-format-1 (blogit--get-filepath-format info))
         (filepath-format (blogit--format-to-s-format filepath-format-1))
         (filepath ""))

    ;; Build dir according to export format
    (setq filepath
          (s-format filepath-format 'aget (blogit--build-format-list info)))

    ;; append a backslash after filepath
    (setq filepath (concat filepath "/"))

    ;; if `#+URL:' has backslah, add it as dir
    (let* ((pdir-str (blogit--get-post-filename info))
           (pdir-1 (split-string pdir-str "/"))
           (pdir (butlast pdir-1)))

      (when pdir
        (dolist (d pdir)
          (setq filepath (concat filepath d))))
      (setq filepath (concat filepath "/")))

    ;; remove dulpicate /
    (blogit--remove-dulpicate-backslash
     (format "%s/%s" (directory-file-name (blogit-project-info :publishing-directory)) filepath))))

;; FIXME: what about ./ ?
(defun blogit--path-to-root (path)
  "Return path to site root.
ex:
    (root)/2013/12/23/test.html -> ../../..
    (root)/theme                -> ..
"
  (let* ((root (expand-file-name (blogit-project-info :publishing-directory)))
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
         (rpath (s-replace (expand-file-name (blogit-project-info :publishing-directory)) "" path-dir)))
    (blogit--remove-dulpicate-backslash
     (concat (blogit--path-to-root  path-dir) "/"  rpath filename))))

(defun blogit--render-template (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (blogit--template-fullfile type) type)))
    (mustache-render (blogit--template-to-string file) context)))

(defun blogit--parse-option (info key &optional filename)
  "Read option value of org file opened in current buffer.

This function will first use the standard way to parse org-option.
If parsing failed, use regexp to get the options, else return nil.
"
  (if filename
      (blogit--file-in-temp-buffer filename (blogit--parse-option info key))
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
                (match-string-no-properties 2 nil))))))))

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
    ("BLOG_TITLE" (or (blogit-project-info :blog-title) ""))
    ("BLOG_DESCRIPTION"  (or (blogit-project-info :blog-description) ""))
    ("TITLE"  ,(or (blogit--parse-option info :title) "Untitled"))
    ("AUTHOR" ,(or (blogit--parse-option info :author) user-full-name "Unknown"))
    ("EMAIL" ,(or (blogit--parse-option info :email) user-mail-address ""))
    ("DATE" ,(or (blogit--parse-option info :date) ""))
    ("URL" ,(or (blogit--parse-option info :url) ""))
    ("LANGUAGE" ,(or (blogit--parse-option info :language) "en"))
    ("DESCRIPTION" ,(or (blogit--parse-option info :description) ""))
    ("KEYWORDS" ,(or (blogit--parse-option info :keywords) ""))
    ("DISQUS" ,(or (blogit--render-disqus-template info) ""))
    ("ANALYTICS" ,(or (blogit--render-analytics-template info) ""))
    ("ROOT" ,(blogit--path-to-root (blogit--build-export-dir info)))
    ,@pairs))


;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit 'html
  :options-alist
  '(
    (:analytics         "ANALYTICS"         nil   nil   (blogit-project-info :google-analytics))
    (:disqus            "DISQUS"            nil   nil   (blogit-project-info :disqus))
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
  (when (or (blogit--parse-option info :disqus) (blogit-project-info :disqus))
    (blogit--render-template
     :plugin_disqus
     (ht ("DISQUS" (or (blogit--parse-option info :disqus) (blogit-project-info :disqus)))))))

(defun blogit--render-analytics-template (info)
  (when (or (blogit--parse-option info :analytics) (blogit-project-info :google-analytics))
    (blogit--render-template
     :plugin_analytics
     (ht ("ANALYTICS" (or (blogit--parse-option info :analytics) (blogit-project-info :google-analytics)))))))

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
        (add-to-list 'blogit-linked-cache file-to-cache))

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


;;; Extra functions for blogit-publish

(defun blogit-publish-linked-file (output-file)
  "Copy all linked file to dst."
  (let ((cache blogit-linked-cache)
        (pub-dir (concat (file-name-directory output-file) (file-name-base output-file))))

    ;; copy file to (blogit-project-info :publishing-directory) if file in cache.
    (dolist (src cache)
      (let ((dst (concat pub-dir "/" (file-name-nondirectory src))))
        (blogit--do-copy src dst)))
    ;; do not forget to clear cache
    (setq blogit-linked-cache nil)))

;; TODO: finish this function
;; TODO: how about add atom support ?
(defun blogit-publish-rss ()
  "Publish rss or atom file for blogit."

  ;; TODO: generate rss contenst, how to?
  (let* ((cache "recents")
         (cache-val (blogit-cache-get cache)))

    ;; since we only get rss/atom length defined in
    ;; `(blogit-project-info :export-rss-number)', reset cache-val length
    (setq cache-val (-take (blogit-project-info :export-rss-number) cache-val))

    ;; pass cache info to create rss
    (blogit--string-to-file
     (blogit--render-template
      :rss
      (blogit--build-context
       nil
       ("ITEMS"
        (--map
         (ht
          ("TITLE"    (plist-get (blogit-cache-get (cdr it))  :title))
          ("POST_URL" (plist-get (blogit-cache-get (cdr it)) :post-url))
          ("DESCRIPTION" (plist-get (blogit-cache-get (cdr it)) :description))
          ("DATE" (plist-get (blogit-cache-get (cdr it)) :date))
          ("POST_LINK" (plist-get (blogit-cache-get (cdr it)) :post-link))
          )
         cache-val))))

     ;; FIXME:
     (blogit--remove-dulpicate-backslash
      (concat (blogit-project-info :publishing-directory) "/" "rss.xml")))
    ))


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
                  (file-name-nondirectory (blogit--get-post-filename nil base-name)))))

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
               (blogit-publish-linked-file output-file)))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))


;;; Caching functions

(defun blogit-write-cache-file (&optional free-cache)
  "Write `blogit-cache' to file.
If FREE-CACHE, empty the cache."
  (unless blogit-cache
    (error "`blogit-write-cache-file' called, but no cache present"))

  (let ((cache-file (blogit-project-info :blogit-cache-file)))
    (unless cache-file
      (error "Cannot find cache-file name in `blogit-write-cache-file'"))
    (with-temp-file cache-file
      (let (print-level print-length)
        (insert "(setq blogit-cache (make-hash-table :test 'equal :weakness nil :size 100))\n")
        (maphash (lambda (k v)
                   (insert
                    (format (concat "(puthash %S "
                                    (if (or (listp v) (symbolp v))
                                        "'" "")
                                    "%S blogit-cache)\n") k v)))
                 blogit-cache)))
    (when free-cache (blogit-reset-cache))))

(defun blogit-initialize-cache ()
  "Initialize the projects cache if not initialized yet and return it."

  (unless (file-exists-p (blogit-project-info :blogit-cache-directory))
    (make-directory (blogit-project-info :blogit-cache-directory) t))
  (unless (file-directory-p (blogit-project-info :blogit-cache-directory))
    (error "Blogit cache: %s is not a directory" (blogit-project-info :blogit-cache-directory)))

  (unless blogit-cache

    (let* ((cache-file
            (expand-file-name (blogit-project-info :blogit-cache-file)))

           (cexists (file-exists-p cache-file)))

      (when blogit-cache (blogit-reset-cache))

      (if cexists (load-file cache-file)
        (setq blogit-cache
              (make-hash-table :test 'equal :weakness nil :size 100))
        (blogit-cache-set ":project:" "publish")
        ;;(blogit-cache-set "tags" '())
        (blogit-cache-set ":blogit-version:" blogit-version))
      (unless cexists (blogit-write-cache-file nil))))
  blogit-cache)

(defun blogit-reset-cache ()
  "Empty org-publish-cache and reset it nil."
  (message "%s" "Resetting blogit-cache")
  (when (hash-table-p blogit-cache)
    (clrhash blogit-cache))
  (setq blogit-cache nil))

(defun blogit-cache-get (key)
  "Return the value stored in `blogit-cache' for key KEY.
Returns nil, if no value or nil is found, or the cache does not
exist."
  (unless blogit-cache
    (error "`blogit-cache-get' called, but no cache present"))
  (gethash key blogit-cache))

(defun blogit-cache-set (key value)
  "Store KEY VALUE pair in `blogit-cache'.
Returns value on success, else nil."
  (unless blogit-cache
    (error "`blogit-cache-set' called, but no cache present"))
  (puthash key value blogit-cache))

(defun blogit-update-cache (filename)
  "Update blogit-cache to log post info."
  (flet ((get-info (key)
                   (list key (blogit--parse-option nil key)))
         (post-url ()
                   (format "%s%s"
                           (s-replace
                            (concat (expand-file-name (blogit-project-info :publishing-directory)) "/") ""
                            (expand-file-name (blogit--build-export-dir nil)))
                           (blogit--get-post-filename nil filename)))
         (post-link ()
                    (blogit--remove-dulpicate-backslash
                     (concat (blogit-project-info :blog-url) "/" (post-url)))))
    (let* ((info
            (blogit--file-in-temp-buffer
             filename
             (-flatten
              (list
               (map 'list 'get-info '(:title :date :language :tags :description))
               :type (blogit--get-post-type nil)
               :post-url (post-url)
               :post-link (post-link)
               )))))

      ;; update fileinfo cache
      (blogit-cache-set filename info)

      ;; update tags cache
      (blogit-update-tags-cache info)

      ;; update recent post cache, this cache also for rss
      (blogit-update-recents-cache info filename)
      )))

;; FIXME: should static page need to be ignore by tags?

(defun blogit-update-tags-cache (info)
  "Build tags info for all files, this function will also count every
tags repeat times."
  (let* ((tags (blogit--get-tags info))
         (cache "tags")
         (cache-val (blogit-cache-get cache)))

    (dolist (tag tags)
      (let ((key (blogit--string-to-key tag)))

        ;; calculate tags count
        (if (member key cache-val)
            (let ((count (plist-get cache-val key)))
              (setq cache-val (plist-put cache-val key (+ count 1))))
          (setq cache-val (plist-put cache-val key 1)))

        ;; add filename to every `tags-name' cache that files has
        (let* ((tag-cache (format "tags-%s" tag))
               (tag-cache-val (blogit-cache-get tag-cache))

               (title (plist-get info :title))
               (post-url (plist-get info :post-url)))

          (blogit-cache-set tag-cache (add-to-list 'tag-cache-val `(,title . ,post-url))))))

    (blogit-cache-set cache cache-val)))

;; TODO:
;; In traditional way, a `static' page should not be log into rss
;; feed, we also prevent add static page in our feed, but what if user
;; want this feature ?

(defun blogit-update-recents-cache (info filename)
  "Build recents post cache, post are store in `anti-chronologically' order.
This cache will be used to build rss and recent post."
  (flet ((anti-chronologically
          (a b)
          (let* ((adate (org-time-string-to-time (car a)))
                 (bdate (org-time-string-to-time (car b)))
                 (A (+ (lsh (car adate) 16) (cadr adate)))
                 (B (+ (lsh (car bdate) 16) (cadr bdate))))
            (>= A B))))

    (let* ((date (plist-get info :date))
           (type (plist-get info :type))
           (cache "recents")
           (cache-val (blogit-cache-get cache)))

      (unless (eq type 'static)

        ;; add current file info to cache and sort by date
        ;; in `anti-chronologically' order.
        (setq cache-val (add-to-list 'cache-val `(,date . ,filename)))
        (setq cache-val (sort cache-val #'anti-chronologically))

        ;; we only take what we need in recents cache
        (setq cache-val (-take
                         (max (blogit-project-info :export-rss-number) (blogit-project-info :export-recents-number))
                         cache-val))

        (blogit-cache-set cache cache-val)))))


;;; Debugging functions

;;;###autoload
(defun blogit-verify-configuration ()
  "Ensure all required configuration fields are properly configured,
include:

`(blogit-project-info :base-directory)'
`(blogit-project-info :publishing-directory)'
`(blogit-project-info :blog-url)'

Blogit will throw error if not properly configure, this will help to debug
the problem."
  (interactive)
  (unless (and (blogit-project-info :base-directory) (file-directory-p (blogit-project-info :base-directory)))
    (error "Variable `%s' is not properly configured or directory does not exist."
           (symbol-name '(blogit-project-info :base-directory))))
  (unless (and (blogit-project-info :publishing-directory) (file-directory-p (blogit-project-info :publishing-directory)))
    (error "Variable `%s' is not properly configured or directory does not exist."
           (symbol-name '(blogit-project-info :publishing-directory))))
  (unless (blogit-project-info :blog-url)
    (error "Variable `%s' is not properly configured."
           (symbol-name '(blogit-project-info :blog-url))))
  (message "Blogit verify configuration SUCCESS!"))


;;; End-user functions

;;;###autoload
(defun blogit-insert-template (&optional filename)
  "Insert blogit newpost template."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (insert
     (blogit--render-template
      :newpost
      (blogit--build-context
       nil
       ("TITLE" (file-name-base (or filename "")))
       ("DATE" (format-time-string (blogit-project-info :blogit-date-format)))
       ("URL" (blogit--sanitize-string filename))
       ("LANGUAGE" (or (blogit-project-info :default-language) "en"))))))
  (end-of-buffer)
  (newline-and-indent))

;;;###autoload
(defun blogit-new-post (filename)
  "Create a new post in `(blogit-project-info :base-directory)'."
  (interactive "sTitle for new post: ")
  (find-file (concat
              (file-name-as-directory (blogit-project-info :base-directory)) filename ".org"))
  (blogit-insert-template filename))

;;;###autoload
(defun blogit-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit Export*\", which
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

    ;; check if file is not under blogit ignore directory
    (dolist (d (blogit-project-info :blogit-ignore-directory-list))
      (if do-publish
          (when (string= file-dir
                       (file-name-directory (expand-file-name (concat (blogit-project-info :base-directory) "/" d "/"))))
	    (setq do-publish nil))))

    ;; if file is draft, do not publish it
    (if do-publish
      (when (eq 'draft (blogit--get-post-type nil filename)) (setq do-publish nil)))

    ;; only publish when do-publish is t
    (when do-publish
      (blogit-publish-org-to 'blogit filename
                             (concat "." (or (plist-get plist :html-extension)
                                             org-html-extension "html"))
                             plist pub-dir)
      ;; Add file info to blogit cache
      (blogit-update-cache filename))))

;;;###autoload
(defun blogit-publish-blog (&optional force)
  "Publush all blogit post, if post already posted and not modified,
skip it.

When force is t, re-publish all blogit project."
  (interactive)

  ;; Initial project info to current project
  ;; TODO: Add multi project support
  (blogit-initialize-project blogit-project-list)

  (let* ((start-time (current-time)) ;; for statistic purposes only
         (org-publish-timestamp-directory (blogit-project-info :blogit-cache-directory))
         (source-style-dir (blogit-project-info :blogit-style-directory))
         (output-dir (blogit-project-info :publishing-directory))
         (output-style-dir (concat output-dir (blogit-project-info :style-directory-name) "/"))
	 copy-style-dir org-publish-cache)

    ;; when republish blogit project, we need to remove
    ;; org-publish-timestamp-directory, which is the same as
    ;; (blogit-project-info :blogit-cache-directory)
    (when (and force
               (file-exists-p org-publish-timestamp-directory))
      (delete-directory org-publish-timestamp-directory t nil)
      ;; reset blogit cache
      (blogit-reset-cache))

    ;; initialize cache for blogit
    (blogit-initialize-cache)

    ;; publish all posts
    ;; TODO: how about create multiple blogit project like org-publish
    ;; do ?

    (org-publish-project blogit-current-project)

    ;; Copy theme-dir to (blogit-project-info :publishing-directory) when publish
    ;; if theme dir does not exit, re-copy again
    ;; when we republish blogit project, also enable
    ;; copy-style-dir

    ;; Copy style dir according to `:copy-style-directory-method',
    ;; when republish blogit posts, always re-copy style dir event it exist.

    (unless (or force
		(eq 'always (blogit-project-info :copy-style-directory-method))
                (not (file-exists-p output-style-dir)))
      (setq copy-style-dir t))

    ;; write cache file
    (blogit-write-cache-file)

    (when copy-style-dir
      (message "Copy style dir to blogit-output-dir.")
      (blogit--do-copy source-style-dir output-dir))

    ;; calculate publish time
    (message (format "All files published in %ss"
                     (format-time-string "%s.%3N"
                                         (time-subtract (current-time) start-time))))))

;;;###autoload
(defun blogit-republish-blog ()
  "Republish all blogit post."
  (interactive)
  (blogit-publish-blog t))

(provide 'blogit)
;;; blogit.el ends here.

;; FIXME:
;; 1. if post does not conatin #+DATE:, we need to add it automatically
;; 2. make template more useful

;; TODO:
;; 1. how about make user can publish many bligit project ?
