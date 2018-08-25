;;; completion/ivy/packages.el -*- lexical-binding: t; -*-

(package! ivy
          counsel
          swiper
          ivy-rich
          ivy-hydra
          counsel-projectile
          ;; smex
          amx)

(when (featurep! +fuzzy)
  (package! flx))
