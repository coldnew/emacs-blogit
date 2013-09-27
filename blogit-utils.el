;;; blogit-utils.el --- .

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

;; (defmacro blogit~check-variables (var type &optional msg)
;;   "Check blogit variable is exit and matching type, include:

;; `:dir': check directory is exist or not.
;; `:file': check file is exist or not.
;; `:other': check var is not null, this config can pass custom msg.

;;   If check without error, return t, else nil.
;; "
;;   `(let ((var-name (if ,(symbolp var) (symbol-name (car-safe ,@var)) "")))
;;      (case ,type
;;        (:dir (unless (and ,var (file-directory-p ,var))
;;                (error "Directory `%s' is not configured or not exist." var-name)))

;;        (:file (unless (and ,var (file-exists-p ,var))
;;                 (error "File `%s' is not configured or not exist."
;;                        ,(symbol-name var))) t)
;;        (:other (unless ,var
;;                  (if (string-or-null-p ,msg)
;;                      (error ,msg ,(symbol-name var))
;;                    (error "Variable `%s' is not configured." ,(symbol-name var)))) t)
;;        ))
;;   )

(defun blogit-file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun blogit-template-to-string (file)
  "Read the content of FILE in template dir and return it as string."
  (blogit-file-to-string (concat blogit-template-dir file)))

(defun blogit-template-render (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (case type
		(:index   blogit-template-index)
		(:header  blogit-template-header)
		(:content blogit-template-content)
		(t type))))
  (mustache-render (blogit-template-to-string file) context)))

(defun blogit-string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major
mode, format the string with MODE's format settings."
  (with-temp-buffer
    (insert string)
    (set-buffer-file-coding-system 'utf-8-unix)
    (when (and mode (functionp mode))
      (funcall mode)
      (flush-lines "^[ \\t]*$" (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max))
      (indent-region (point-min) (point-max)))
    (when (file-writable-p file)
      (write-region (point-min) (point-max) file))))



(defmacro blogit~generate-file (template dir)
  "Generate file from template to dir with same name."
  `(copy-file ,template
              (expand-file-name ,template dir)))

(defun blogit~check-variables (var type &optional msg)
  "Check blogit variable is exit and matching type, include:

`:dir': check directory is exist or not.
`:file': check file is exist or not.
`:not-nil': check var is not null, this config can pass custom msg.

  If check without error, return t, else nil.
"
  (let ((var-name  (if (symbolp var) (symbol-name var) var))
        (var-value (if (symbolp var) (symbol-value var) var)))
    (case type
      (:dir (unless (and var-value (file-directory-p var-value))
              (error "Directory `%s' is not configured or not exist." var-name)) t)
      (:file (unless (and var-value (file-exists-p var-value))
               (error "File `%s' is not configured or not exist." var-name)) t)
      (:not-nil (unless var-value
                  (if (string-or-null-p msg) (error msg var-name))
                  (error "Variable `%s' is not configured." var-name)) t))
    ))


(defun blogit-do-copy (src dst &optional copyf args)
  "Copy SRC into DST. If `dired-do-sync' is found it would be
preferred. Otherwise, `copy-directory' or `copy-files' would be
used.

A copy function COPYF and its arguments ARGS could be specified."
  (let* ((dirp (file-directory-p src))
	 (copyf (cond
		 (copyf copyf)
		 ((functionp 'dired-do-sync) 'dired-do-sync)
		 (dirp 'copy-directory)
		 (t 'copy-file)))
	 (args (or args
		   (when (eq 'copy-file copyf) '(t t t)))))
    (when (file-exists-p src)
	(apply copyf src dst args))))

(provide 'blogit-utils)
