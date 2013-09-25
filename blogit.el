;;; blogit.el --- .

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
;; 	M-x package-install blogit
;;
;; In your .emacs
;;
;; 	(require 'blogit)

;;; Code:

(require 'ox)
(require 'ht)

(require 'blogit-utils)
(require 'blogit-vars)
(require 'blogit-create)

(defvar blogit-version "0.1"
  "Blogit version string.")

(defvar blogit-generator-string
  (concat "emacs-blogit ver. " blogit-version)
  "Generator string to indicate blogit version.")

;; TODO:
(defun blogit-publish-html ()
  "Publish blog from org file to html files."
  )

(defun blogit-create-site (dir)
  "Create new static site for blogit."
  (interactive (read-directory-name
		"Specify a directory for blogit site: " nil nil nil))
  (mkdir (expand-file-name dir t))
  (blogit-generate-readme dir)
  (blogit-generate-about dir)
  (blogit-generate-index dir))

(defun blogit-publish-changes ()
"1. Publish org file th html
 2. update index pages"

)


(provide 'blogit)
