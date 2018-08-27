;;; completion/helm/packages.el -*- lexical-binding: t; -*-

(package! helm
          helm-ag
          ;; helm-descbinds
          helm-projectile
          helm-swoop)

(when (featurep! +fuzzy)
  (package! helm-flx))
