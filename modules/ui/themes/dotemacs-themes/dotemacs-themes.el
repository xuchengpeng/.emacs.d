;;; dotemacs-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng
;; Package-Requires: ((emacs "24.4")(cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Flagship themes
;;   `dotemacs-one'
;;   `dotemacs-one-light'
;;   `dotemacs-vibrant'
;;
;; Additional themes
;;   `dotemacs-city-lights' (added by fuxialexnder)
;;   `dotemacs-darcula' (added by fuxialexnder)
;;   `dotemacs-molokai'
;;   `dotemacs-nord' (added by fuxialexnder)
;;   `dotemacs-nord-light' (added by fuxialexnder)
;;   `dotemacs-opera' (added by jwintz)
;;   `dotemacs-opera-light' (added by jwintz)
;;   `dotemacs-nova' (added by bigardone)
;;   `dotemacs-peacock' (added by teesloane)
;;   `dotemacs-solarized-light' (added by fuxialexnder)
;;   `dotemacs-spacegrey' (added by teesloane)
;;   `dotemacs-tomorrow-night'
;;   `dotemacs-tomorrow-day'
;;   `dotemacs-mono-dark' / `dotemacs-mono-light'
;;   `dotemacs-tron'
;;
;;
;; A comprehensive configuration example:
;;
;;   (require 'dotemacs-themes)
;;
;;   ;; Global settings (defaults)
;;   (setq dotemacs-themes-enable-bold t    ; if nil, bold is universally disabled
;;         dotemacs-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;;   ;; Load the theme (dotemacs-one, dotemacs-molokai, etc); keep in mind that each
;;   ;; theme may have their own settings.
;;   (load-theme 'dotemacs-one t)
;;
;;   ;; Enable flashing mode-line on errors
;;   (dotemacs-themes-visual-bell-config)
;;
;;
;;; Code:

(require 'cl-lib)
(require 'dotemacs-themes-common)

(defgroup dotemacs-themes nil
  "Options for dotemacs-themes."
  :group 'faces)

(defface dotemacs-modeline-error '((t (:inherit error :inverse-video t)))
  "Face to use for the mode-line when `dotemacs-themes-visual-bell-config' is used."
  :group 'dotemacs-themes)

;;
(defcustom dotemacs-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'dotemacs-themes
  :type 'boolean)

(defcustom dotemacs-themes-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'dotemacs-themes
  :type 'boolean)

(defcustom dotemacs-themes-padded-modeline nil
  "Default value for padded-modeline setting for themes that support it."
  :group 'dotemacs-themes
  :type '(or integer boolean))

(define-obsolete-variable-alias 'dotemacs-enable-italic 'dotemacs-themes-enable-italic "1.2.9")
(define-obsolete-variable-alias 'dotemacs-enable-bold   'dotemacs-themes-enable-bold "1.2.9")

(defvar dotemacs-themes--colors nil)
(defvar dotemacs-themes--inhibit-warning nil)
(defvar dotemacs-themes--bell-p nil)


;; Color helper functions
;; Shamelessly *borrowed* from solarized
;;;###autoload
(defun dotemacs-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
(defun dotemacs-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (dotemacs-blend (dotemacs-color color1) (dotemacs-color color2) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (dotemacs-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (dotemacs-name-to-rgb color1)
                           for other in (dotemacs-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (t color1))))

;;;###autoload
(defun dotemacs-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (dotemacs-darken (dotemacs-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (dotemacs-darken c alpha)))

        (t
         (dotemacs-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun dotemacs-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (dotemacs-lighten (dotemacs-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (dotemacs-lighten c alpha)))

        (t
         (dotemacs-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(defun dotemacs-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name dotemacs-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

;;;###autoload
(defun dotemacs-ref (face prop &optional class)
  "TODO"
  (let ((spec (or (cdr (assq face dotemacs-themes--faces))
                  (error "Couldn't find the '%s' face" face))))
    (when (memq (car spec) '(quote backquote \`))
      (user-error "Can't fetch the literal spec for '%s'" face))
    (when class
      (setq spec (cdr (assq class spec)))
      (unless spec
        (error "Couldn't find the '%s' class in the '%s' face"
               class face)))
    (unless (plist-member spec prop)
      (error "Couldn't find the '%s' property in the '%s' face%s"
             prop face (if class (format "'s '%s' class" class) "")))
    (plist-get spec prop)))

;;;###autoload
(defmacro dotemacs-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES."
  (declare (indent defun))
  `(custom-theme-set-faces
    ,theme
    ,@(mapcar #'dotemacs-themes--build-face faces)))

(defmacro def-dotemacs-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a dotemacs theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((dotemacs-themes--colors defs))
    `(let* ((bold   dotemacs-themes-enable-bold)
            (italic dotemacs-themes-enable-italic)
            ,@defs)
       (setq dotemacs-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces
        ',name ,@(dotemacs-themes-prepare-facelist extra-faces))
       (custom-theme-set-variables
        ',name ,@(dotemacs-themes-prepare-varlist extra-vars))
       (provide-theme ',name))))

;;;###autoload
(defun dotemacs-themes-org-config ()
  "Enable custom fontification and improves dotemacs-themes integration with org-mode."
  (require 'dotemacs-themes-org))

;;;###autoload
(defun dotemacs-themes-treemacs-config ()
  "Install dotemacs-themes' treemacs configuration."
  (require 'dotemacs-themes-treemacs))

;;;###autoload
(defun dotemacs-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'dotemacs-themes-visual-bell-fn
        visible-bell t))

;;;###autoload
(defun dotemacs-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (unless dotemacs-themes--bell-p
    (let ((old-remap (copy-alist face-remapping-alist)))
      (setq dotemacs-themes--bell-p t)
      (setq face-remapping-alist
            (append (delete (assq 'mode-line face-remapping-alist)
                            face-remapping-alist)
                    '((mode-line dotemacs-modeline-error))))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (remap buf)
                        (with-current-buffer buf
                          (when (assq 'mode-line face-remapping-alist)
                            (setq face-remapping-alist remap
                                  dotemacs-themes--bell-p nil))
                          (force-mode-line-update)))
                      old-remap
                      (current-buffer)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
