;;; blogit-render.el --- .

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
(require 'mustache)
(require 'blogit-utils)
(require 'blogit-vars)
(require 'blogit-create)


(let ((context (ht ("name" "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))


(defun blogit-render-header ()
  "Render the header on each page."
  (blogit-template-render
   :header
   (ht ("TITLE"  (or (blogit-parse-option "TITLE") "Untitled"))
       ("AUTHOR" (or (blogit-parse-option "AUTHOR") user-full-name "Unknown Author"))
       ("GENERATOR" blogit-generator-string)
       ("DESCRIPTION" (or (blogit-parse-option "DESCRIPTION") ""))
       ("KEYWORDS" (or (blogit-parse-option "KEYWORDS") "")
        ))))

;; (defun blogit-render-content ()
;;   (mustache-render
;;    (blogit-template-to-string blogit-template-content)
;;    (ht ("title" (or (op/read-org-option "TITLE") "Untitled"))
;;        ("content" (org-export-as 'html nil nil t nil)))))

(defun blogit-render-post ()
  "Render full post."
  (blogit-template-render
   :content
   (ht ("HEADER" (blogit-render-header))
       ("TITLE" (or (blogit-parse-option "TITLE") "Untitled"))
       ("CONTENT" (org-export-as 'html nil nil t nil))
       )))


(defun blogit-generate-url ()
  ()
  (let ((date-str
         (or (blogit-parse-option "DATE")
             (blogit-modify-option "DATE" (format-time-string blogit-date-format)))))
    ;; create
    (concat
     (directory-file-name blogit-output-dir) "/"
     (directory-file-name (if date-str date-str
                            (blogit-parse-option "DATE"))) "/"
                            (or (blogit-parse-option "URL")
                                (blogit-sanitize-string (file-name-base
                                                         (buffer-file-name (current-buffer)))))
                            ".html")))


(provide 'blogit-render)
