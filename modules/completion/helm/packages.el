;;; completion/helm/packages.el -*- lexical-binding: t; -*-

(package! helm)
(package! helm-ag)
(package! helm-descbinds)
(package! helm-projectile)
(package! helm-swoop)
(package! swiper-helm)

(when (featurep! +fuzzy)
  (package! helm-flx))
