;;; blogit-cache.el --- Cache manipulate function for blogit.

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
(require 'dired-sync nil t)

(require 'blogit-vars)

(eval-when-compile (require 'cl))


;;; Caching functions

(defun blogit-write-cache-file (&optional free-cache)
  "Write `blogit-cache' to file.
If FREE-CACHE, empty the cache."
  (unless blogit-cache
    (error "`blogit-write-cache-file' called, but no cache present"))

  (let ((cache-file (blogit-project-info :blogit-cache-file)))
    (unless cache-file
      (error "Cannot find cache-file name in `blogit-write-cache-file'"))
    (with-temp-file cache-file
      (let (print-level print-length)
        (insert "(setq blogit-cache (make-hash-table :test 'equal :weakness nil :size 100))\n")
        (maphash (lambda (k v)
                   (insert
                    (format (concat "(puthash %S "
                                    (if (or (listp v) (symbolp v))
                                        "'" "")
                                    "%S blogit-cache)\n") k v)))
                 blogit-cache)))
    (when free-cache (blogit-reset-cache))))

(defun blogit-initialize-cache ()
  "Initialize the projects cache if not initialized yet and return it."

  (unless (file-exists-p (blogit-project-info :blogit-cache-directory))
    (make-directory (blogit-project-info :blogit-cache-directory) t))
  (unless (file-directory-p (blogit-project-info :blogit-cache-directory))
    (error "Blogit cache: %s is not a directory" (blogit-project-info :blogit-cache-directory)))

  (unless blogit-cache

    (let* ((cache-file
            (expand-file-name (blogit-project-info :blogit-cache-file)))

           (cexists (file-exists-p cache-file)))

      (when blogit-cache (blogit-reset-cache))

      (if cexists (load-file cache-file)
        (setq blogit-cache
              (make-hash-table :test 'equal :weakness nil :size 100))
        (blogit-cache-set ":project:" "publish")
        (blogit-cache-set ":blogit-version:" blogit-version))
      (unless cexists (blogit-write-cache-file nil))))
  blogit-cache)

(defun blogit-reset-cache ()
  "Empty org-publish-cache and reset it nil."
  (message "%s" "Resetting blogit-cache")
  (when (hash-table-p blogit-cache)
    (clrhash blogit-cache))
  (setq blogit-cache nil))

(defun blogit-cache-get (key)
  "Return the value stored in `blogit-cache' for key KEY.
Returns nil, if no value or nil is found, or the cache does not
exist."
  (unless blogit-cache
    (error "`blogit-cache-get' called, but no cache present"))
  (gethash key blogit-cache))

(defun blogit-cache-set (key value)
  "Store KEY VALUE pair in `blogit-cache'.
Returns value on success, else nil."
  (unless blogit-cache
    (error "`blogit-cache-set' called, but no cache present"))
  (puthash key value blogit-cache))

;; FIXME:
;; When use blogit-publish, some cache may vanish

(file-relative-name "sample-init.el" (blogit--parse-option nil :url))

(defun blogit-update-cache (filename)
  "Update blogit-cache to log post info."
  (flet ((get-info (key)
                   (list key (or (blogit--parse-option nil key) "")))
         (post-url ()
                   (format "%s%s"
                           (s-replace
                            (expand-file-name (blogit-project-info :publishing-directory)) ""
                            (expand-file-name (blogit--build-export-dir nil)))
                           (blogit--get-post-filename nil filename)))
         (post-link ()
		    (concat (blogit-project-info :blog-url) "/" (post-url)))
	 (post-content ()
		       (blogit--file-in-temp-buffer
			filename
			(blogit--modify-option "OPTIONS"
					       (concat (or (blogit--parse-option nil :options) "") " toc:nil"))
			(org-export-as 'blogit-html nil nil t nil))))
    (let* ((info
            (blogit--file-in-temp-buffer
             filename
             (-flatten
              (list
               (map 'list 'get-info '(:title :date :language :tags))
               :type (blogit--get-post-type nil)
               :post-url (post-url)
               :post-link (post-link)
	       :atom (post-content)
	       :rss (post-content)
               )))))

      ;; update fileinfo cache
      (blogit-cache-set filename info)

      ;; update tags cache
      (blogit-update-tags-cache info)

      ;; update recent post cache, this cache also for rss
      (blogit-update-recents-cache info filename)
      )))

;; FIXME: should static page need to be ignore by tags?

(defun blogit-update-tags-cache (info)
  "Build tags info for all files, this function will also count every
tags repeat times."
  (let* ((tags (blogit--get-tags info))
         (cache ":tags:")
         (cache-val (blogit-cache-get cache)))

    (dolist (tag tags)
      (let ((key (blogit--string-to-key tag)))

        ;; add filename to every `tags-name' cache that files has
        (let* ((tag-cache (format ":tags-%s:" tag))
               (tag-cache-val (blogit-cache-get tag-cache))

               (title (plist-get info :title))
               (post-url (plist-get info :post-url)))

          (add-to-list 'tag-cache-val `(,title . ,post-url))
          (blogit-cache-set tag-cache tag-cache-val)

          ;; calculate tags count
          (if (member key cache-val)
              (let ((count (length (blogit-cache-get tag-cache))))
                (setq cache-val (plist-put cache-val key count)))
            (setq cache-val (plist-put cache-val key 1))))))

    (blogit-cache-set cache cache-val)))

;; TODO:
;; In traditional way, a `static' page should not be log into rss
;; feed, we also prevent add static page in our feed, but what if user
;; want this feature ?

(defun blogit-update-recents-cache (info filename)
  "Build recents post cache, post are store in `anti-chronologically' order.
This cache will be used to build rss and recent post."
  (flet ((anti-chronologically
          (a b)
          (let* ((adate (org-time-string-to-time (car a)))
                 (bdate (org-time-string-to-time (car b)))
                 (A (+ (lsh (car adate) 16) (cadr adate)))
                 (B (+ (lsh (car bdate) 16) (cadr bdate))))
            (>= A B))))

    (let* ((date (plist-get info :date))
           (type (plist-get info :type))
           (cache ":recents:")
           (cache-val (blogit-cache-get cache)))

      (unless (eq type 'static)

        ;; add current file info to cache and sort by date
        ;; in `anti-chronologically' order.
        (add-to-list 'cache-val `(,date . ,filename))
        (setq cache-val (sort cache-val #'anti-chronologically))

        ;; we only take what we need in recents cache
        (setq cache-val (-take
                         (max (blogit-project-info :export-rss-number) (blogit-project-info :export-recents-number))
                         cache-val))

        (blogit-cache-set cache cache-val)))))


(provide 'blogit-cache)
;;; blogit-cache.el ends here.
