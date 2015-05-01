;;; blogit.el --- Export org-mode to pelican.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(eval-when-compile (require 'cl-lib))

(require 'noflet)
(require 'f)
(require 'ox-html)
(require 'ox-publish)

;;;; Group

(defgroup blogit nil
  "."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/coldnew/emacs-blogit"))

;;;; Customize

(defcustom blogit-template-directory
  (f-join (file-name-directory (or load-file-name (buffer-file-name))) "template")
  "Get the absolute path of template directory. This directory contains some template for create new project."
  :group 'blogit
  :type 'string)

;;;; Vars

(defvar blogit-project-alist '())

(defvar blogit--current-project nil)



;;;; Backend

(org-export-define-derived-backend 'blogit-pelican 'html
  :translate-alist
  '(
    (template . org-blogit-template)
    ;; Fix for multibyte language
    (paragraph . org-blogit-paragraph)
    ;; Fix toc for blogit theme
    (inner-template . org-blogit-inner-template)
    )
  :options-alist
  '((:date "DATE" nil nil)
    )
  )


;;;; Paragraph

(defun org-blogit-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Fix multibyte language like chinese which will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.
  (let* ((fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents))
         ;; Unfill paragraph to make contents look mode better
         (unfill-contents
          (with-temp-buffer
            (insert fix-contents)
            (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil (point-min) (point-max))
            (buffer-string))))

    ;; Send modify data to org-html-paragraph
    (org-html-paragraph paragraph unfill-contents info)))


;;; Template

(defun org-blogit-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-blogit-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))


;;; Tables of Contents

(defun org-blogit-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth))))
    (when toc-entries
      (format "<div class=\"table-of-contents\">\n\n"))))


;;; Template

(defun blogit--parse-date (info)
  (let ((date (car (plist-get info :date))))
    (if (stringp date)
        ;; backward compability with blogit
        date
      ;; parse org-timestamp
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (apply 'encode-time (org-parse-time-string
                                               (org-element-property :raw-value date)))))))


(defun org-blogit--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
         (lambda (str)
           (replace-regexp-in-string
            "\"" "&quot;" (org-html-encode-plain-text str))))
        (title (org-export-data (plist-get info :title) info))
        (author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth
                            ;; Return raw Org syntax, skipping non
                            ;; exportable objects.
                            (org-element-interpret-data
                             (org-element-map auth
                                 (cons 'plain-text org-element-all-objects)
                               'identity info))))))
        (date (blogit--parse-date info))
        (category (plist-get info :category))
        (tags (plist-get info :tags))
        (save-as (plist-get info :save_as))
        (url (plist-get info :url)))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
        (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (org-html-close-tag "meta" " name=\"generator\" content=\"blogit\"" info)
     "\n"
     (and (org-string-nw-p author)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"author\" content=\"%s\""
                                       (funcall protect-string author))
                               info)
           "\n"))
     (and (org-string-nw-p date)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"date\" content=\"%s\"\n"
                                       (funcall protect-string date)
                                       )
                               info)
           "\n")
          )
     (and (org-string-nw-p category)
          (concat
           (org-html-close-tag "meta"
                               (format " name=\"category\" content=\"%s\""
                                       (funcall protect-string category))
                               info)
           "\n")))))

(defun org-blogit-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (org-html-doctype info)
   "\n"
   (concat "<html"
           (when (org-html-xhtml-p info)
             (format
              " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
              (plist-get info :language) (plist-get info :language)))
           ">\n")
   "<head>\n"
   (org-blogit--build-meta-info info)
   (org-html--build-head info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
         (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
               (or link-up link-home)
               (or link-home link-up))))

   ;; Document contents.
   (format "<%s id=\"%s\">\n"
           (nth 1 (assq 'content org-html-divs))
           (nth 2 (assq 'content org-html-divs)))

   contents
   (format "</%s>\n"
           (nth 1 (assq 'content org-html-divs)))

   ;; Closing document.
   "</body>\n</html>"))



;;; End-user functions

;;;###autoload
(defun blogit-export-as-pelican
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'blogit-pelican "*Blogit perlican Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun blogit-publish-pelican (&optional force)
  )

;;;###autoload
(defun blogit-republish-pelican (&optional force)
  )


;;;###autoload
(defun org-blogit-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blogit-pelican filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))

(setq aa '("asd" ("sad" "asd")))


(setq blogit-project-alist nil)

(add-to-list 'blogit-project-alist
             '("coldnew's blog"
               :publishing-directory "~/Workspace/blog/test/content/"
               :org-publish-project "blog"
               :org-publish-project-alist
               (("base-orgs" ;; an identifier
                 :base-directory "~/Workspace/blog/blog-src/blog" ;; path where I put the articles and pages
                 :base-extension "org" ;; export org files
                 :publishing-function org-blogit-publish-to-html
                 :auto-sitemap nil ;; don't generate a sitemap (kind of an index per folder)
;;                 :publishing-directory "~/Workspace/blog/test/content/" ;; where to publish those files
                 :recursive t ;; recursively publish the files
                 :headline-levels 4 ;; Just the default for this project.
                 :auto-preamble nil ;; Don't add any kind of html before the content
                 :export-with-tags t
                 :todo-keywords nil
                 :html-doctype "html5" ;; set doctype to html5
                 :html-html5-fancy t
                 :creator-info nil ;; don't insert creator's info
                 :auto-postamble nil ;; Don't add any kind of html after the content
                 :html-postamble nil ;; same thing
                 :timestamp nil ;;
                 ;; just in case we don't want to publish some part of the files
                 :exclude-tags ("noexport" "todo"))
                ("blog-static" ;; identifier for static files
                 :base-directory  "~/Workspace/blog/blog-src" ;; path where I put the articles and pages
;;                 :publishing-directory "~/Workspace/blog/test/content" ;; where to publish those files
                 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                 :recursive t
                 ;; method to use for publishing those files
                 :publishing-function org-publish-attachment)
                ;; meta identifier to publish everything at once
                ("blog" :components ("base-orgs" "blog-static")))
               ))

;; (let ((org-publish-project-alist blogit-project-alist))
;;   (org-publish  "blog"))


(defun blogit--select-project (func &optional msg)
  "Prompt to select project to publish. If only one project
list in `blogit-project-alist', do not prompt."
  (let ((project
         (if (= (length blogit-project-alist) 2)
             (list (cadr blogit-project-alist))
           (list
            (assoc (org-icompleting-read
                    (or msg "Publish blogit project: ")
                    blogit-project-alist nil t)
                   blogit-project-alist)
            current-prefix-arg))))
    (let ((project-list
           (if (not (stringp project)) (list project)
             ;; If this function is called in batch mode,
             ;; project is still a string here.
             (list (assoc project org-publish-project-alist)))))
      (mapc
       (lambda (current-project)
         ;; Initial project info to current project
         ;;  (blogit-initialize-project current-project)
         ;; NOTE: we will initial cache again at
         ;; `blogit--publish-blog'. This will help us to prevent cache
         ;; mismatch to current project.
         ;;         (setq blogit-cache nil)
         ;;         (blogit-initialize-cache)
         (funcall func current-project))

       (org-publish-expand-projects (car project-list))))))

(blogit--select-project nil)

;;;###autoload
(defun blogit-publish (&optional force)
  "Published modified blogit files."
  (interactive)
  ;;  (blogit--select-project 'blogit--publish-blog)

  )


(provide 'blogit)
;;; blogit.el ends here.
