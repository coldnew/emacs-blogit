;;; blogit-bootstrap.el --- bootstrap support in blogit.

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

(require 'blogit-vars)
(require 'blogit-core)


(defun blogit-bootstrap-publish-source (info val)
  "Publish an sourced file in HTML mode.

A source file is defined using:

    #+BLOGIT_SOURCE: path/to/file [mode]

and is converted according to bootstrap-source templatate.
"
  (let* ((val-list (split-string val " "))
         (file (car val-list))
         (mode (cadr val-list))
         (contents (or (blogit--file-to-string file) "")))
    (blogit--render-template
     :bootstrap_source
     (blogit--build-context
      info
      ("FILENAME" file)
      ("FILENAME_BASE" (file-name-base file))
      ("FILENAME_SANITIZE" (blogit--sanitize-string (file-name-base file)))
;;      ("CONTENT" (org-html-src-block src-block contents info))
      ("CONTENT" (blogit--file-to-source-html file mode))
      )))
  )

(defun blogit--file-to-source-html (file &optional mode)
  (let ((src-block
         (with-temp-buffer
          (goto-char (point-min))

          (insert  (if mode (format "#+BEGIN_SRC %s\n" mode)
                     (format "#+BEGIN_EXAMPLE\n")))
	  (insert-file-contents file)

          (goto-char (point-max))

          (insert  (if mode (format "#+END_SRC\n")
                     (format "#+END_EXAMPLE\n")))
	  (org-export-as 'html nil nil t nil)))

	)
    src-block))

(provide 'blogit-bootstrap)
