;;; blogit-vars.el --- Variables defined in blogit

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

(require 'ht)
(require 'dash)
(require 's)
(require 'with-namespace)

(eval-when-compile (require 'cl))


;;; User Configuration Variables

(setq blogit-default-project-alist
      '("Blogit"

        ;; same options as `org-publist-project-alist'
        :base-directory ""
        :publishing-directory ""
        :recursive t
        :base-extension "org"

        ;; extra options defined in blogit
        :default-language "en"
        :template-directory-name "templates"
        :style-directory-name    "style"
        :tags-directory-name "tags"

        :always-copy-style-directory t

        :google-analytics ""
        :disqus    ""
        :lloogg    ""

        :blog-url   ""
        :blog-title ""
        :blog-description ""

        :type-list blogit-type-list
        :template-list blogit-template-list
        :publishing-function org-blogit-publish-to-html

        ;; Rss support
        :export-rss t
        :export-rss-number 10
        :rss-filename "rss.xml"
        ;;        :rss-filepath "" ;; TODO:

        ;; Atom support
        :export-atom t
        :export-atom-number 10
        :atom-filename "atom.xml"
        ;;        :atom-filepath "" ;; TODO:

        :export-recents-post t          ; TODO:
        :export-recents-number 10

        ;; hooks
        :before-publish-hook nil
        :after-publish-hook nil

        ;; Advanced options for customize blogit
        ;; some will be set by `blogit-initialize-project'
        :blogit-sanitize-length 5
        :blogit-date-format "%Y-%02m-%02d %02H:%02M:%02S"
        :blogit-ignore-directory-list nil

        :blogit-cache-directory ""
        :blogit-cache-directory-name ".cache"
        :blogit-cache-file ""

        :blogit-style-directory ""
        :blogit-template-directory ""
        :blogit-tags-directory ""
        :blogit-default-type blog
        ))

(defvar blogit-project-alist nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

1. A well-formed property list with an even number of elements,
   alternating keys and values, specifying parameters for the
   publishing process.

     \(:property value :property value ... )

2. A meta-project definition, specifying of a list of
   sub-projects:

     \(:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after `:components'
are taken to be components of the project, which group together
files requiring different publishing options.  When you publish
such a project with \\[org-publish], the components all publish.

When a property is given a value in `org-publish-project-alist',
its setting overrides the value of the corresponding user
variable (if any) during publishing.  However, options set within
a file override everything.

Most properties are optional, but some should always be set:

  `:base-directory'

    Directory containing publishing source files.

  `:publishing-directory'

    Directory (possibly remote) where output files will be
    published.

  `:blog-url'

    Url for link to the static site you publish. You should set
    this option if enable rss/atom feed export.

Some option you may set to make blog more complete:

   `:blog-title'

    The main title for your blog.

   `:blog-description'

    Description for your blog.

"
  )

;; FIXME: not let anyone modified this ?
(defvar blogit-type-list
  (list
   :draft  '(:type draft)
   :blog   '(:type blog   :root "blog" :filepath "%y/%m"  :filename "%d_%s.html")
   :static '(:type static :root ""     :filepath ""       :filename "%s.html")
   :note   '(:type note   :root "note" :filepath ""       :filename "%s.html"))
  "Output dir formate for blogit, use `(blogit-project-info :publishing-directory)' as root when
this value is empty string.

The dir will be format by `format-time-string', but the time is according to
your #+DATE info.

Currently blogit only support following format:

    %y : year   eq: 2013
    %m : month  eq: 03
    %d : day    eq: 23
    %S : filename without any sanitize. `NOTE: Not suggest use this.'
    %s : filename sanitize by `blogit--sanitize-string', if `#+URL' is specified, use it.
         When `#+URL' contains backslash, this fotmat will be ignore.
")

(defvar blogit-template-list
  (list
   :page_header        "page_header.html"
   :page_navigator     "page_navigator.html"
   :page_footer        "page_footer.html"
   :plugin_analytics   "plugin_analytics.html"
   :plugin_disqus      "plugin_disqus.html"
   :plugin_lloogg      "plugin_lloogg.html"
   :plugin_qrcode      "plugin_qrcode.html"
   :plugin_fancybox    "plugin_fancybox.html"
   :rss                "rss.xml"
   :atom               "atom.xml"
   :newpost            "newpost.org"

   ;; TODO: new template
   :note_post          "note_post.html"

   ;; combine other templates
   :blog_post          "blog_post.html"
   :static_post        "static_post.html"
   :tag                "tag.html"
   :tag_index          "tag_index.html"

   ;; bootstrap support
   :bootstrap_source   "bootstrap_source.html"
   )
  "Template filename define for blogit to parse.")


;;; Internal variables

(defvar blogit-linked-cache nil
  "Cache to store which file will be copied to output dir.")

(defvar blogit-current-project nil
  "Cache to store current project plist info.")

(defvar blogit-tags-list nil
  "Cache to store all need refresh tag list for tag page.")

(defvar blogit-current-file nil
  "Cache to store current processing file path.")

(defvar blogit-cache nil
  "Cache to store post info, this cache will be used to
generate rss and tage.")


(defvar a '(
            :type-draft nil
                        :type-note nil
                        :type-blog nil
                        :type-static nil
                        :oo
                        ))


(provide 'blogit-vars)
;;; blogit-vars.el ends here.
