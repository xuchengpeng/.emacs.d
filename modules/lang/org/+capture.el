;;; lang/org/+capture.el -*- lexical-binding: t; -*-

(after-load! 'org-capture  
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
               '("tt" "Task" entry
                 (file+headline "task.org" "Task")
                 "* TODO %^{Task name}\n%u\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tw" "Work Task" entry
                 (file+headline "work.org" "Work")
                 "* TODO %^{Task name}\n%u\n%a\n" :clock-in t :clock-resume t)))
