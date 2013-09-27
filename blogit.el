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
;;      M-x package-install blogit
;;
;; In your .emacs
;;
;;      (require 'blogit)

;;; Code:

(require 'ox)
(require 'ht)
(require 'dash)
(require 's)

;; import extra files in blogit
(mapcar (lambda (x) (require (intern (format "blogit-%s" x)) nil t))
        '("utils" "vars" "create" "test" "export"))


(defconst blogit-version "0.1"
  "Blogit version string.")

(defconst blogit-generator-string
  (concat "emacs-blogit ver. " blogit-version)
  "Generator string to indicate blogit version.")

;;; Customize Variables

(defgroup blogit nil
  "Options for generating static pages using blogit."
  :tag "Org static page generator" :group 'org)

(defcustom blogit-source-dir nil
  "The source directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-output-dir nil
  "The output directory for blogit."
  :group 'blogit :type 'string)

(defcustom blogit-site-domain nil
  "The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned."
  :group 'blogit :type 'string)

(defcustom blogit-date-format "%Y-%m-%d"
  "Format for printing a date in the sitemap.
See `format-time-string' for allowed formatters."
  :group 'blogit :type 'string)

;;; Templates

(defcustom blogit-template-dir "templates/"
  "Template directory."
  :group 'blogit :type 'string)

(defcustom blogit-template-header "header.html"
  "Template for generate html header."
  :group 'blogit :type 'string)

(defcustom blogit-template-content "post.html"
  "Template for generate html contents."
  :group 'blogit :type 'string)

(defcustom blogit-template-rss "rss.html"
  "Template for generate rss files."
  :group 'blogit :type 'string)




(defun blogit-publish-current-file ()
  (interactive)
  (let ((out-dir (file-name-directory (blogit-generate-url))))
    ;; create dir for output files
    (mkdir out-dir t)
    ;; generate file
    (blogit-string-to-file
     (blogit-render-post)
     (blogit-generate-url))))


(provide 'blogit)

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;;
