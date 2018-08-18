;;; lang/org/+capture.el -*- lexical-binding: t; -*-

(after-load! 'org-capture
  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Hugo Posts" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "hugo-posts.org" "Hugo Posts")
                 (function dotemacs-org-hugo-new-subtree-post-capture-template)))
  
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
               '("tt" "Task" entry
                 (file+headline "task.org" "Task")
                 "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tw" "Work Task" entry
                 (file+headline "work.org" "Work")
                 "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t)))
