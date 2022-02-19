;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! '(vertico :type git :host github :repo "minad/vertico"
                             :files ("*.el" "extensions/*.el")))

(package! orderless)
(package! consult)
(package! embark)
(package! embark-consult)
(package! marginalia)
