;;; lang/org/+publish.el -*- lexical-binding: t; -*-

(use-package ox-publish
  :commands (org-publish-project)
  :config
  (setq org-html-validation-link nil
        org-publish-timestamp-directory (concat dotemacs-cache-dir "org-timestamps/"))

  (setq org-publish-project-alist
        '(;; Publish the posts
          ("blog-notes"
           :base-directory "~/org/blog"
           :base-extension "org"
           :publishing-directory "~/org/public"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :with-author nil
           :with-creator nil
           :with-email nil
           :with-timestamps nil
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\"/>"
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           ; :html-preamble nil
           ; :html-postamble nil
           :html-link-home "index.html"
           :html-link-up "sitemap.html"
           :htmlized-source t
           :auto-sitemap t
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           )

          ;; For static files that should remain untouched
          ("blog-static"
           :base-directory "~/org/blog"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
           :publishing-directory "~/org/public"
           :recursive t
           :publishing-function org-publish-attachment
           )

          ;; Combine the two previous components in a single one
          ("blog" :components ("blog-notes" "blog-static"))))
  )
