;;; blogit-vars.el --- .

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


(defconst blogit-temp-buffer-name "*Blogit Output*"
  "Name of the temporary buffer used by blogit.")


(defcustom blogit-site-main-title "blogit"
  "The main title of entire site."
  :group 'blogit :type 'string)

(defcustom blogit-site-sub-title "static site generator"
  "The subtitle of entire site."
  :group 'blogit :type 'string)

(defcustom blogit-personal-github-link "https://github.com/coldnew/blogit"
  "The personal github link."
  :group 'blogit :type 'string)

(defcustom blogit-personal-disqus-shortname nil
  "The personal disqus shortname."
  :group 'blogit :type 'string)

(defcustom blogit-personal-google-analytics-id nil
  "Personal google analytics id."
  :group 'blogit :type 'string)


(defcustom blogit-template-readme "README.md"
  "Template to generate README.md for new blogit project."
  :group 'blogit :type 'string)

(defcustom blogit-template-index "index.org"
  "Template to generate index.org for new blogit project."
  :group 'blogit :type 'string)

(defcustom blogit-template-about "about.org"
  "Template to generate about.org for new blogit project."
  :group 'blogit :type 'string)


(provide 'blogit-vars)
