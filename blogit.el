;;; blogit.el --- Export org-mode to pelican.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/emacs-blogit
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2"))

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
(require 's)

;;;; Group

(defgroup blogit nil
  "."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/coldnew/emacs-blogit"))

;;;; Customize

(defcustom blogit-project-alist nil
  "Project list for blogit, it can contains many project, all
  project must config like following:

(setq blogit-project-alist
      '((\"coldnew's blog\" :config \"~/Workspace/blog/config.el\")))
"
  :group 'blogit
  :type 'list)

(defcustom blogit-clear-ouput-when-republish nil
  "When t, clean files in `blogit-ouput-directory' when republish project."
  :group 'blogit
  :type 'bool)


;;;; Default Variables

(defcustom blogit-default-template-directory
  (f-join (file-name-directory (or load-file-name (buffer-file-name))) "template")
  "Get the absolute path of template directory. This directory contains some template for create new project.")

(defconst blogit-default-template-list
  (list
   :newpost  "newpost.org"
   ))


;;;; Internal Variables

(defvar blogit-template-directory nil
  "This variable should be defined in user's blogit config.el.")

(defvar blogit-output-directory nil
  "This variable should be defined in user's blogit config.el.")

(defvar blogit-cache-directory nil
  "Change org-mode's `~/.org-timestamp' storage path.This
  variable should be defined in user's blogit config.el.")

(defvar blogit-cache-filelist nil
  "List to storage where to remove the cache file. This variable will be rebuilf in `blogit--select-project'")

(defvar blogit-publish-project-alist nil
  "This variable should be defined in user's blogit config.el.")

(defvar blogit-before-publish-hook nil
  "Hook before publish blogit files.")

(defvar blogit-after-publish-hook nil
  "Hook after publish blogit files.")

(defvar blogit-before-republish-hook nil
  "Hook before republish blogit files.")

(defvar blogit-after-republish-hook nil
  "Hook after republish blogit files.")

(defvar blogit-tempate-list nil
  )


;;;; Internal Functions
(defun blogit--clear-private-variables ()
  "Clear all private variables in blogit."
  (setq blogit-output-directory nil
        blogit-cache-directory nil
        blogit-cache-filelist nil
        blogit-publish-project-alist nil
        blogit-before-publish-hook nil
        blogit-after-publish-hook nil
        blogit-before-republish-hook nil
        blogit-after-republish-hook nil
        ;; restore to default value
        blogit-template-directory blogit-default-template-directory
        blogit-tempate-list nil
        ))

(defun blogit--select-project (func &optional msg)
  "Prompt to select project to publish. If only one project
list in `blogit-project-alist', do not prompt."
  (let ((project
         (if (= (length blogit-project-alist) 1)
             (list (car blogit-project-alist))
           (list
            (assoc (org-icompleting-read
                    (or msg "Publish blogit project: ")
                    blogit-project-alist nil t)
                   blogit-project-alist)
            current-prefix-arg))))
    ;; clear all private variable before load config file.
    (blogit--clear-private-variables)
    ;; load config according blogit-project-list
    (let ((config (plist-get (cdar project) :config)))
      (if config (load config)
        (error (format "config %s not exist") config)))

    (let ((project-list (list blogit-publish-project-alist)))
      ;; rebuild cache filelist
      (setq blogit-cache-filelist nil)
      (dolist (c (car project-list))
        (add-to-list
         'blogit-cache-filelist
         (f-join blogit-cache-directory (concat (car c) ".cache"))))

      (mapc
       (lambda (current-project)
         (funcall func current-project))

       (org-publish-expand-projects project-list)))))

(defun blogit--publish-project (project &optional force)
  "Publush all blogit post, if post already posted and not modified,
skip it.

When force is t, re-publish all blogit project."
  (let ((org-publish-project-alist project)
        (org-publish-timestamp-directory
         (file-name-as-directory blogit-cache-directory)))

    (run-hooks 'blogit-before-publish-hook)

    ;; when repiblish blogit project, we need to remove all already exist cache
    ;; file store in `blogit-cache-filelist'
    (when force
      (run-hooks 'blogit-before-republish-hook)
      (dolist (c blogit-cache-filelist)
        (if (file-exists-p c) (delete-file c)))
      ;; if option on, clean all files in `blogit-output-directory'.
      (when blogit-clear-ouput-when-republish
        (let ((target-dir
               (cond
                ;; if target is symlink, remove symlink dir and recreate it
                ((f-symlink? blogit-output-directory) (file-symlink-p blogit-output-directory))
                ;; delete directory and recreate it
                ((f-directory? blogit-output-directory) blogit-output-directory)
                (t (error "BUG: unknown remove blogit-output-directory methd.")))))
          ;; delete target-dir and recreate it
          (f-delete target-dir t)
          (f-mkdir target-dir)))

      (run-hooks 'blogit-after-republish-hook))

    ;; publish all current project
    (org-publish-all force)

    (run-hooks 'blogit-after-publish-hook)))

(defun blogit--new-post (project)
  "Create new post according to project."
  (let ((filename (read-input "Title for new post: ")))
    (find-file (concat

                ))
    ;;(blogit--insert-newpost-template filename)
    )
  )

(defun blogit--insert-newpost-template (&optional filename)
  "Insert blogit newpost template."
  (save-excursion
    (widen)
    (goto-char (point-min))
    ;;    (insert)
    )
  (end-of-buffer)
  (newline-and-indent))


;;; Extra functions

(defun blogit-string-to-key (string)
  "Conver string to key. eq: \"test\" -> :test"
  (intern (format ":%s" string)))

(defun blogit-symbol-to-key (symbol)
  "Conver symbol to key. eq: test -> :test"
  (blogit-string-to-key (symbol-name symbol)))

(defun blogit-key-to-string (key)
  "Conver key to string. eq: :test -> \"test\""
  (let ((key-str (symbol-name key)))
    (s-right (- (length key-str) 1) key-str)))

(defun blogit-key-to-symbol (key)
  "Conver key to symbol. eq: test -> :test"
  (intern (blogit-key-to-string key)))

(defun blogit-set-option (key value)
  "Modify option value of org file opened in current buffer.
If option does not exist, create it automatically."
  (let* ((option (upcase (blogit-key-to-string key)))
         (match-regexp (org-make-options-regexp `(,option)))
         (blank-regexp "^#\\+\\(\\w*\\):[        ]*\\(.*\\)")
         (insert-option '(insert (concat "#+" option ": " value)))
         (mpoint))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (progn
            (goto-char (point-at-bol))
            (kill-line)
            (eval insert-option))
        ;; no option found, insert it
        (progn
          (goto-char (point-min))
          (while (re-search-forward blank-regexp nil t)
            (setq mpoint (point)))
          (if (not mpoint) (setq mpoint (point-min)))
          (goto-char mpoint)
          (when (not (= mpoint (point-min)))
            (goto-char (point-at-eol))
            (newline-and-indent))
          (eval insert-option)
          (if (= mpoint (point-min))
              (newline-and-indent))
          )))))

(defun blogit-get-option (key)
  "Read option value of org file opened in current buffer.

This function will first use the standard way to parse org-option.
If parsing failed, use regexp to get the options, else return nil.
"
  (let* ((option (upcase (blogit-key-to-string key)))
         (match-regexp (org-make-options-regexp `(,option))))

    ;; use regexp to find options
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))


;;; End-user functions

;;;###autoload
(defun blogit-publish (&optional force)
  "Published modified blogit files."
  (interactive)
  (blogit--select-project 'blogit--publish-project))

;;;###autoload
(defun blogit-republish (&optional force)
  "Force republish project."
  (interactive)
  (noflet ((blogit--republish-project
            (project-list)
            (blogit--publish-project project-list t)))
    (blogit--select-project 'blogit--republish-project)))

;;;###autoload
(defun blogit-new-post ()
  "Create a new post according to project and templates."
  (interactive)
  (blogit--select-project 'blogit--new-post))

;;;###autoload
(defun blogit-insert-newpost-template (&optional filename)
  "Insert blogit newpost template."
  (interactive)
  (blogit--select-project 'blogit--insert-newpost-template))

(provide 'blogit)
;;; blogit.el ends here.
