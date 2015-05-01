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
  (let ((meta-project-name-alist '()))
    ;; build meta project name alist
    (dolist (p blogit-project-alist)
      (let ((name (car p))
            (meta? (plist-get (cdr p) :components)))
        (if meta?
            (add-to-list 'meta-project-name-alist name))))

    ;; FIXME: if no project name find, throw error

    ;; ask for project to publish
    (let ((project-name
           (org-icompleting-read
            (or msg "Publish blogit project:")
            meta-project-name-alist nil t)))

      (funcall func project-name))))


;;; End-user functions

;;;###autoload
(defun blogit-publish (&optional force)
  "Published modified blogit files."
  (interactive)
  ;; TODO: enable force rebuild project
  (let ((org-publish-project-alist blogit-project-alist))
    (blogit--select-project 'org-publish)))



;;; Test


(setq blogit-project-alist
      '(
        ("base-orgs" ;; an identifier
         :base-directory "~/Workspace/blog/blog-src/blog" ;; path where I put the articles and pages
         :base-extension "org" ;; export org files
         :publishing-function org-pelican-publish-to-html
         :auto-sitemap nil ;; don't generate a sitemap (kind of an index per folder)
         :publishing-directory "~/Workspace/blog/test/content/article" ;; where to publish those files
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
        ("blog-static" ;; identifier for static files
         :base-directory  "~/Workspace/blog/blog-src" ;; path where I put the articles and pages
         :publishing-directory "~/Workspace/blog/test/content" ;; where to publish those files
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-function org-publish-attachment ;; method to use for publishing those files
         )
        ("blog-asd" :components ("base-orgs")) ;; meta identifier to publish everything at once
        ("blog2" :components ("base-orgs" "blog-static")) ;; meta identifier to publish everything at once
        ))


(provide 'blogit)
;;; blogit.el ends here.
