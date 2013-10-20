;;; blogit-core.el --- Core utils for blogit.

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

(require 'ox-publish)
(require 'ht)
(require 'dash)
(require 's)
(require 'mustache)
(require 'dired-sync nil t)

(require 'blogit-vars)

(eval-when-compile (require 'cl))


;;; Internal general purpose functions

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

(defun blogit--string-list-to-plist (str)
  "Convert string-list to plist, for example:

   (\":a test :b another test\")

will be convert to

   (:a \"test\" :b \"another test\")
"
  (let ((old-element "")
        (tmp-plist
         (mapcar (lambda (x)
                   (if (s-starts-with? ":" x) (intern x) x))
                 (split-string str " ")))
        new-plist)
    (dolist (e tmp-plist)
      (if (stringp e)
          (progn
            (if (string= old-element "")
                (setq old-element e)
              (setq old-element (concat old-element " " e))))
        (progn
          (unless (string= old-element "")
            (add-to-list 'new-plist old-element))
          (add-to-list 'new-plist e)
          (setq old-element ""))))
    (unless (string= old-element "")
      (add-to-list 'new-plist old-element))
    (nreverse new-plist)))


;;; Internal Project Control functions

(defun blogit-combine-project (project)
  (let ((project-name (car project))
        (project-plist (cdr project)))
    (cons project-name
          (org-combine-plists
           (cdr blogit-default-project-alist) project-plist))))

(defun blogit-project-info (key)
  "Return project info according to key."
  (if (eq key :project)
      (car blogit-current-project)
    (plist-get (cdr blogit-current-project) key)))

(defun blogit--plist-remove (plist &rest keys)
  (let ((proper-plist plist))
    ;; FIXME: possible optimization: (blogit--plist-remove '(:x 0 :a 1 :b 2) :a)
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
        (blogit-combine-project project-list))

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

;; FIXME: This match method may contains some error

(defun blogit--swtich-to-file-project (file)
  "Switch to file's relative project if file is under
project list. Return nil when failed."
  (let ((ret))
    (dolist (p blogit-project-alist)
      (let* ((info (cdr p))
             (base-directory (plist-get info :base-directory))
             (project-directory
              (convert-standard-filename
               (blogit--remove-dulpicate-backslash
                (concat base-directory "/")))))

        (when (blogit--file-in-dir-p project-directory file)
          ;; Initialize project info
          (blogit-initialize-project p)
          (blogit-initialize-cache)
	  (setq ret t))))
    ret))


;;; Internal functions

(defmacro blogit--file-in-temp-buffer (filename &rest pairs)
  "Helper macro to open file in temp buffer, and execute blogit function."
  `(when (file-exists-p ,filename)
     (with-temp-buffer (insert-file-contents ,filename) ,@pairs)))

(defun blogit--file-in-dir-p (file dir)
  "Check if file is under directory."
  (let ((file-dir (file-name-directory (expand-file-name file))))
    (string= file-dir
             (file-name-directory (expand-file-name dir)))))

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
                     (blogit--sanitize-string
                      filename
                      (blogit-project-info :blogit-sanitize-length)))))
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

(defun blogit--get-post-date-list (info &optional filename)
  "Get post date in list. This function will return like

   (:year \"2013\" :month \"10\" :day \"12\")"
  (blogit--parse-date-string
   (blogit--parse-option info :date filename)))

(defun blogit--get-tags (info &optional filename)
  "Get blogit tags in list from info or filename."
  (let* ((tag-opt (or (blogit--parse-option info :tags filename) ""))
         (tags (split-string tag-opt " ")))
    ;; remove empty string and dulpicate tag
    (remove-duplicates (remove "" tags) :test 'string=)))

;; FIXME: how about remove this ?
(defun blogit--template-to-string (file)
  "Read the content of FILE in template dir and return it as string."
  (blogit--file-to-string file))

(defun blogit--template-fullfile (key)
  "Get match template filename with fullpath according to key."
  (convert-standard-filename
   (blogit--remove-dulpicate-backslash
    (concat (blogit-project-info :blogit-template-directory) "/"
            (plist-get blogit-template-list key)))))

(defun blogit--sanitize-string (s &optional length)
  "Sanitize string S by:

- converting all charcters to pure ASCII
- replacing non alphanumerical by the first char of sha1 algorithm
- downcasing all letters
- trimming leading and tailing \"_\"

This function is used to generate blog post url if not specified."
  (if length (s-left length (blogit--sanitize-string s))
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
                           (mapconcat 'identity ret ""))))))

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

;; FIXME: rename to `blogit--build-export-dirrectory'
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

(defun blogit--build-post-url (info &optional filename)
  (format "%s%s"
          (s-replace
           (expand-file-name (blogit-project-info :publishing-directory)) ""
           (expand-file-name (blogit--build-export-dir info)))
          (blogit--get-post-filename info filename)))

(defun blogit--build-post-link (info &optional filename)
  (concat (blogit-project-info :blog-url) "/" (blogit--build-post-url info filename)))

(defun blogit--build-post-file-directory (info &optional filename)
  (file-name-base (blogit--get-post-filename info filename)))

(defun blogit--build-feed-content (info &optional filename)
  ;; FIXME: fix file
  (replace-regexp-in-string
   (concat "<img src=\"" (blogit--build-post-file-directory info filename))
   (concat "<img src=\"" (file-name-directory (blogit--build-post-url info filename))
	   (blogit--build-post-file-directory info filename))
   (blogit--file-in-temp-buffer
    filename
    (blogit--modify-option "OPTIONS"
			   (concat (or (blogit--parse-option nil :options) "") " toc:nil"))
    (org-export-as 'blogit-html nil nil t nil))))

;; FIXME: what about ./ ?
(defun blogit--path-to-root (path)
  "Return path to site root.
ex:
    (root)/2013/12/23/test.html -> ../../..
    (root)/theme                -> ..
"
  (let* ((root (blogit-project-info :publishing-directory))
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


;;; Blogit context builder and template render

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
    ("TITLE"  (or (blogit--parse-option ,info :title) "Untitled"))
    ("AUTHOR" (or (blogit--parse-option ,info :author) user-full-name "Unknown"))
    ("EMAIL" (or (blogit--parse-option ,info :email) user-mail-address ""))
    ("DATE" (or (blogit--parse-option ,info :date) ""))
    ("POST_DAY" (or (plist-get (blogit--get-post-date-list ,info) :day) ""))
    ("POST_MONTH" (or (plist-get (blogit--get-post-date-list ,info) :month) ""))
    ("POST_YEAR" (or (plist-get (blogit--get-post-date-list ,info) :year) ""))
    ("CURRENT_DATE" (format-time-string (blogit-project-info :blogit-date-format)))
    ("CURRENT_YEAR" (format-time-string "%Y"))
    ("CURRENT_MONTH" (format-time-string "%02m"))
    ("CURRENT_DAY" (format-time-string "%02d"))
    ("BLOG_URL" (blogit--build-post-link ,info))
    ("BLOG_URL_HEXIFY" (url-hexify-string (blogit--build-post-link ,info)))
    ("URL" (or (blogit--parse-option ,info :url) ""))
    ("LANGUAGE" (or (blogit--parse-option ,info :language) "en"))
    ("DESCRIPTION" (or (blogit--parse-option ,info :description) ""))
    ("KEYWORDS" (or (blogit--parse-option ,info :keywords) ""))
    ("DISQUS" (or (blogit--render-disqus-template ,info) ""))
    ("ANALYTICS" (or (blogit--render-analytics-template ,info) ""))
    ("LLOOGG" (or (blogit--render-lloogg-template ,info) ""))
    ("ROOT" (blogit--path-to-root (blogit--build-export-dir ,info)))
    ,@pairs))

;; TODO: seems like we can reduce some function here.

(defun blogit--render-template (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (blogit--template-fullfile type) type)))
    (mustache-render (blogit--template-to-string file) context)))

(defun blogit--render-header-template (info)
  (blogit--render-template :page_header (blogit--build-context info)))

(defun blogit--render-navigator-template (info)
  (blogit--render-template :page_navigator (blogit--build-context info)))

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

(defun blogit--render-lloogg-template (info)
  (when (or (blogit--parse-option info :lloogg) (blogit-project-info :lloogg))
    (blogit--render-template
     :plugin_lloogg
     (ht ("LLOOGG" (or (blogit--parse-option info :lloogg) (blogit-project-info :lloogg)))))))

(defun blogit--render-qrcode-template (info)
    (blogit--render-template :plugin_qrcode (blogit--build-context info)))

(provide 'blogit-core)
;;; blogit-core.el ends here.
