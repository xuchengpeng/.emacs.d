;;; completion/ivy/packages.el -*- lexical-binding: t; -*-

(package! ivy)
(package! counsel)
(package! swiper)
(package! ivy-rich)
(package! ivy-hydra)
(package! counsel-projectile)
; (package! smex)
(package! amx)

(if (modulep! +prescient)
    (package! ivy-prescient)
  (when (modulep! +fuzzy)
    (package! flx)))

(package! ivy-xref)
