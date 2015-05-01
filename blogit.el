;;; blogit.el --- Export org-mode to pelican.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2") (org-pelican "0.1"))

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
(require 'ox-pelican)

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

(defcustom blogit-project-alist
  '()
  "Project alist like org-project-alist. The meta project need :components key."
  :type 'list)


;;;; Internal Functions

(defun blogit--select-project (func &optional msg)
  "Prompt to select project to publish. If only one project
list in `blogit-project-alist', do not prompt."
  (let ((project
         (if (= (length blogit-project-alist) 1)
             (list (car blogit-project-alist))
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
             (list (assoc project blogit-project-alist)))))
      ;;      (message (format "--->%s" project))
      ;;      (message (format "--->%s" project-list))
      (mapc
       (lambda (current-project)
         (message (format "-000000-->%s" (cdr current-project)))
         ;;         (message (format "--->%s" current-project))
         (funcall func (cdr current-project))
         )
       (org-publish-expand-projects (car project-list))))))


;;; End-user functions

;;;###autoload
(defun blogit-publish (&optional force)
  "Published modified blogit files."
  (interactive)
  ;; TODO: enable force rebuild project
  (let ((org-publish-project-alist blogit-project-alist))
    ;;    (blogit--select-project '(lambda (x) (message (format "%s" x))))
    ;;    (blogit--select-project 'org-publish-all)
    (blogit--select-project
     '(lambda (x);; (message (format "%s" x))
        (let ((org-publish-project-alist x)
              ;;(org-publish-timestamp-directory (blogit-project-info :blogit-cache-directory))
              )
          (org-publish-all force)))
     )))



;;; Test

(setq coldnew-blog-alist
      '("coldnew's blog" ;; project name
        ;; without this, it will use `~/.org-timestamp' as cache
        ;; :cache-directory "~/Workspace/blog/test/content/"
        ("article" ;; an identifier
         :base-directory "~/Workspace/blog/src/article" ;; path where I put the articles and pages
         :base-extension "org" ;; export org files
         :publishing-function org-pelican-publish-to-html
         :auto-sitemap nil ;; don't generate a sitemap (kind of an index per folder)
         :publishing-directory "~/Workspace/blog/content" ;; where to publish those files
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
         :exclude-tags ("noexport" "todo")) ;; just in case we don't want to publish some part of the files
        ("static" ;; identifier for static files
         :base-directory  "~/Workspace/blog/src/data" ;; path where I put the articles and pages
         :publishing-directory "~/Workspace/blog/content" ;; where to publish those files
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-function org-publish-attachment ;; method to use for publishing those files
         )
        ))

(setq blogit-project-alist nil)
(add-to-list 'blogit-project-alist coldnew-blog-alist)



(provide 'blogit)
;;; blogit.el ends here.
