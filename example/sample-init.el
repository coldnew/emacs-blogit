(require 'blogit)

(setq blogit-project-alist
      '(("blogit"
         :base-directory "The source directory"
         :publishing-directory "Directory you want to place output file"
         :blog-url "Your blog url"
         :blog-title "Blog title"

         :default-language "The most use language in your post"

         :google-analytics "Google analytics id"
         :disqus    "Disqus short name"
         :lloogg    "lloogg id"

         :after-publish-hook my/blogit-after-publish-hook
         )))

(defvar my/blogit-after-publish-hook nil)

(defun my/blogit-compile-less-source ()
  (let ((output-dir (blogit-project-info :publishing-directory))
        (output-style-dir
         (concat output-dir "/" (blogit-project-info :style-directory-name)))
        (input-file
         (concat output-style-dir "/source/blogit.less"))
        (output-file
         (concat output-style-dir "/source/blogit.css"))
        (lessc
         (concat output-style-dir "/less.js/bin/lessc")))

    ;; compile blog.less to blog.css and add it to css folder
    (shell-command (concat lessc " -x " input-file " > " output-file))))

(add-hook 'my/blogit-after-publish-hook 'my/blogit-compile-less-source)
