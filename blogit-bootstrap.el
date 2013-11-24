;;; blogit-bootstrap.el --- bootstrap support in blogit.

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html blog org-mode
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.3

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

(require 'blogit-vars)
(require 'blogit-core)

(defun blogit-bootstrap-publish-source (info val)
  "Publish an sourced file in HTML mode.

A source file is defined using:

    #+BLOGIT_SOURCE: :file path/to/file [:mode mode]

and is converted according to bootstrap-source templatate.
"
  (let* ((val-list (split-string val " "))
	 (element (blogit--string-list-to-plist val))
	 (file (expand-file-name
		(concat (file-name-directory blogit-current-file) (plist-get element :file))))
	 (mode (plist-get element :mode))
	 (title (or (plist-get element :title) (file-name-nondirectory file)))
   	 (contents (or (blogit--file-to-string file) "")))
    (blogit--render-template
     :bootstrap_source
     (blogit--build-context
      info
      ("FILENAME" (file-name-nondirectory file))
      ("FILENAME_BASE" (file-name-base file))
      ("FILENAME_SANITIZE" (blogit--sanitize-string (file-name-base file)))
      ("TEXT" (or (plist-get element :text) title))
      ("TITLE" title)
      ("CONTENT" (blogit--file-to-source-html file mode))
      ("BTN" (or (plist-get element :btn) "btn-info"))))))

(defun blogit--file-to-source-html (file &optional mode)
  (let ((src-block
         (blogit--file-in-temp-buffer
	  file
           (goto-char (point-min))
           (insert  (if mode (format "#+BEGIN_SRC %s\n" mode)
                      (format "#+BEGIN_EXAMPLE\n")))

           (goto-char (point-max))
           (insert  (if mode (format "#+END_SRC\n")
                      (format "#+END_EXAMPLE\n")))
           (org-export-as 'html nil nil t nil))))
    src-block))

(provide 'blogit-bootstrap)
