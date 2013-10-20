;;; blogit-publish.el ---

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

;;;; Load blogit functions
(require 'blogit-vars)
(require 'blogit-core)
(require 'blogit-cache)
(require 'blogit-html)

(eval-when-compile (require 'cl))


;;; Extra functions for blogit-publish

(defun blogit-publish-linked-file (output-file)
  "Copy all linked file to dst."
  (let ((cache blogit-linked-cache)
        (pub-dir (concat (file-name-directory output-file) (file-name-base output-file))))

    ;; copy file to (blogit-project-info :publishing-directory) if file in cache.
    (dolist (src cache)
      (let ((dst (concat pub-dir "/" (file-name-nondirectory src))))
        (blogit--do-copy src dst)))
    ;; do not forget to clear cache
    (setq blogit-linked-cache nil)))

(defun blogit-publish-rss/atom ()
  "Publish rss or atom file for blogit."

  (let* ((cache ":recents:")
         (cache-val (blogit-cache-get cache))
         (feed-list (list
                     (if (blogit-project-info :export-rss)  :rss)
                     (if (blogit-project-info :export-atom) :atom))))

    (dolist (f feed-list)
      (let* ((feed-name (blogit--key-to-string f))
	    (feed-file-name
	     (blogit-project-info
              (blogit--string-to-key (format "%s-filename" feed-name))))
            (feed-number
             (blogit-project-info
              (blogit--string-to-key (format "export-%s-number" feed-name)))))
        ;; since we only get rss/atom length defined in
        ;; `(blogit-project-info :export-rss-number)', reset cache-val length
        (setq cache-val (-take feed-number cache-val))

        ;; pass cache info to create rss
        (blogit--string-to-file
         (blogit--render-template
	  f
          (blogit--build-context
           nil
           ("ITEMS"
            (--map
             (ht
              ("TITLE"    (plist-get (blogit-cache-get (cdr it))  :title))
              ("POST_URL" (plist-get (blogit-cache-get (cdr it)) :post-url))
              ("CONTENT" (plist-get (blogit-cache-get (cdr it)) f))
              ("DATE" (plist-get (blogit-cache-get (cdr it)) :date))
              ("POST_LINK" (plist-get (blogit-cache-get (cdr it)) :post-link)))
             cache-val))))

         ;; FIXME: add option to optimize this
         (blogit--remove-dulpicate-backslash
          (concat (blogit-project-info :publishing-directory) "/" feed-file-name)))
	))))


;;; Rewrite some org function to make blogit work more properly

(defun blogit-export-output-file-name (extension &optional subtreep pub-dir)
  "Return output file's name according to buffer specifications.

EXTENSION is a string representing the output file extension,
with the leading dot.

With a non-nil optional argument SUBTREEP, try to determine
output file's name by looking for \"EXPORT_FILE_NAME\" property
of subtree at point.

When optional argument PUB-DIR is set, use it as the publishing
directory.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return file name as a string.

This function is rewrite from `org-export-output-file-name'."
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
         (pub-dir (blogit--build-export-dir nil))
         (base-name
          ;; File name may come from EXPORT_FILE_NAME subtree
          ;; property, assuming point is at beginning of said
          ;; sub-tree.
          (file-name-sans-extension
           (or (and subtreep
                    (org-entry-get
                     (save-excursion
                       (ignore-errors (org-back-to-heading) (point)))
                     "EXPORT_FILE_NAME" t))
               ;; File name may be extracted from buffer's associated
               ;; file, if any.
               (and visited-file (file-name-nondirectory visited-file))
               ;; Can't determine file name on our own: Ask user.
               (let ((read-file-name-function
                      (and org-completion-use-ido 'ido-read-file-name)))
                 (read-file-name
                  "Output file: " pub-dir nil nil nil
                  (lambda (name)
                    (string= (file-name-extension name t) extension)))))))
         (output-file
          ;; Build file name.  Enforce EXTENSION over whatever user
          ;; may have come up with.  PUB-DIR, if defined, always has
          ;; precedence over any provided path.
          (concat (file-name-as-directory (blogit--build-export-dir nil))
                  (file-name-nondirectory (blogit--get-post-filename nil base-name)))))

    ;; If output dir does not exist, create it
    (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))

    ;; If writing to OUTPUT-FILE would overwrite original file, append
    ;; EXTENSION another time to final name.
    (if (and visited-file (org-file-equal-p visited-file output-file))
        (concat output-file extension)
      output-file)))

(defun blogit-publish-org-to (backend filename extension plist &optional pub-dir)
  "Publish an Org file to a specified back-end.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.

Optional argument PUB-DIR, when non-nil is the publishing
directory.

Return output file name.

This function is rewrite from `org-publish-org-to'."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename))))
    (prog1 (with-current-buffer work-buffer
             (let ((output-file
                    (blogit-export-output-file-name extension nil pub-dir))
                   (body-p (plist-get plist :body-only)))

               (org-export-to-file backend output-file
                 nil nil nil body-p
                 ;; Add `org-publish-collect-numbering' and
                 ;; `org-publish-collect-index' to final output
                 ;; filters.  The latter isn't dependent on
                 ;; `:makeindex', since we want to keep it up-to-date
                 ;; in cache anyway.
                 (org-combine-plists
                  plist
                  `(:filter-final-output
                    ,(cons 'org-publish-collect-numbering
                           (cons 'org-publish-collect-index
                                 (plist-get plist :filter-final-output))))))
               ;; copy all needed file for output-file
               (blogit-publish-linked-file output-file)))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))

(defun org-blogit-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML for blogit.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((do-publish t))

    ;; check if file is not under blogit ignore directory
    (dolist (d (blogit-project-info :blogit-ignore-directory-list))
      (if do-publish
          (when
              (blogit--file-in-dir-p filename
                                     (concat (blogit-project-info :base-directory) "/" d "/"))
            (setq do-publish nil))))

    ;; if file is draft, do not publish it
    (if do-publish
        (when (eq 'draft (blogit--get-post-type nil filename))
          (setq do-publish nil)))

    ;; only publish when do-publish is t
    (when do-publish
      ;; Store current process file to blogit-current-file
      (setq blogit-current-file filename)

      (blogit-publish-org-to 'blogit-html filename
                             (concat "." (or (plist-get plist :html-extension)
                                             org-html-extension "html"))
                             plist pub-dir)
      ;; Add file info to blogit cache
      (blogit-update-cache filename)
      ;; clear cache
      (setq blogit-current-file nil))))


;;; Extra functions for End-user functions

(defun blogit--select-project (func &optional msg)
  (let ((project
         (list
          (assoc (org-icompleting-read
                  (or msg "Publish blogit project: ")
                  blogit-project-alist nil t)
                 blogit-project-alist)
          current-prefix-arg)))
    (let ((project-list
           (if (not (stringp project)) (list project)
             ;; If this function is called in batch mode,
             ;; project is still a string here.
             (list (assoc project org-publish-project-alist)))))
      (mapc
       (lambda (current-project)
         ;; Initial project info to current project
         (blogit-initialize-project current-project)
         ;; NOTE: we will initial cache again at
         ;; `blogit--publish-blog'. This will help us to prevent cache
         ;; mismatch to current project.
         (blogit-initialize-cache)
         (funcall func current-project))
       (org-publish-expand-projects (car project-list))))))

(defun blogit--insert-newpost-template (&optional filename)
  "Insert blogit newpost template."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (insert
     (blogit--render-template
      :newpost
      (blogit--build-context
       nil
       ("TITLE" (file-name-base (or filename (buffer-file-name) "")))
       ("DATE" (format-time-string (blogit-project-info :blogit-date-format)))
       ("URL" (blogit--sanitize-string filename))
       ("LANGUAGE" (or (blogit-project-info :default-language) "en"))))))
  (end-of-buffer)
  (newline-and-indent))

(defun blogit--publish-blog (project-list &optional force)
  "Publush all blogit post, if post already posted and not modified,
skip it.

When force is t, re-publish all blogit project."

  (let* ((start-time (current-time)) ;; for statistic purposes only
         (org-publish-timestamp-directory (blogit-project-info :blogit-cache-directory))
         (source-style-dir (blogit-project-info :blogit-style-directory))
         (output-dir (blogit-project-info :publishing-directory))
         (output-style-dir (concat output-dir (blogit-project-info :style-directory-name) "/"))
         org-publish-cache)

    ;; when republish blogit project, we need to remove
    ;; org-publish-timestamp-directory, which is the same as
    ;; (blogit-project-info :blogit-cache-directory)
    (when (and force
               (file-exists-p org-publish-timestamp-directory))
      (delete-directory org-publish-timestamp-directory t nil)
      ;; reset blogit cache
      (blogit-reset-cache))

    ;; initialize cache for blogit
    (blogit-initialize-cache)

    ;; publish current project posts
    (org-publish-project blogit-current-project)

    ;; write cache file
    (blogit-write-cache-file)

    ;; publish rss
    (blogit-publish-rss/atom)

    ;; Copy style dir according to `:always-copy-style-directory',
    ;; when republish blogit posts, always re-copy style dir event it exist.
    (when (or force
              (blogit-project-info :always-copy-style-directory)
              (not (file-exists-p output-style-dir)))
      (message (format "Copy style dir to %s." output-dir))
      (blogit--do-copy source-style-dir output-dir))

    ;; calculate publish time
    (message (format "All files published in %ss"
                     (format-time-string "%s.%3N"
                                         (time-subtract (current-time) start-time))))))


(provide 'blogit-publish)
;;; blogit-publish.el ends here.
