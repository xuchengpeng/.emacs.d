;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(defvar dotemacs|plantuml-jar-path (concat dotemacs-local-dir "plantuml.jar"))

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode))
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path dotemacs|plantuml-jar-path
        org-plantuml-jar-path plantuml-jar-path))
