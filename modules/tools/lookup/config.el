;;; tools/lookup/config.el -*- lexical-binding: t -*-

(use-package ivy-xref
  :when (featurep! :completion ivy)
  :after xref
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package helm-xref
  :when (featurep! :completion helm)
  :after xref
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))
