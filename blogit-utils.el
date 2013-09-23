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
