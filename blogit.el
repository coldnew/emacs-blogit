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

;; TODO: migrate some function to f.el


;;; Blogit info

(defconst blogit-version "0.2"
  "Blogit version string.")

(defconst blogit-url
  "http://github.com/coldnew/emacs-blogit"
  "Url for blogit.")

;;;; Load all blogit functions
(mapcar (lambda (x) (require (intern (format "blogit-%s" x)) nil t))
	'("vars" "core" "bootstrap" "html" "cache" "publish"))


;;; End-user functions

;; FIXME: since we re-ask user the project want to use, this is abit
;; annoying

;;;###autoload
(defun blogit-insert-template (&optional filename)
  "Insert blogit newpost template."
  (interactive)
  (blogit--select-project 'blogit--insert-newpost-template))

;;;###autoload
(defun blogit-new-post ()
  "Create a new post according to project."
  (interactive)
  (flet ((create-new-post-file
          (project)
          (let ((filename (read-input "Title for new post: ")))
            (find-file (concat
                        (file-name-as-directory (blogit-project-info :base-directory)) filename ".org"))
            (blogit--insert-newpost-template filename))))

    (blogit--select-project 'create-new-post-file)))

;;;###autoload
(defun blogit-publish (&optional force)
  (interactive)
  (blogit--select-project 'blogit--publish-blog))


;;;###autoload
(defun blogit-republish (&optional force)
  (interactive)
  (flet ((blogit--republish-blog (project-list)
                                 (blogit--publish-blog project-list t)))
    (blogit--select-project 'blogit--republish-blog)))

(provide 'blogit)
;;; blogit.el ends here.

;; FIXME:
;; 1. if post does not conatin #+DATE:, we need to add it automatically
;; 2. make template more useful

;; TODO:
;; 1. how about make user can publish many bligit project ?
