;;; dotemacs-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 xuchengpeng
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
;;   `dotemacs-city-lights'
;;   `dotemacs-dracula'
;;   `dotemacs-fairy-floss'
;;   `dotemacs-gruvbox'
;;   `dotemacs-Iosvkem'
;;   `dotemacs-molokai'
;;   `dotemacs-moonlight'
;;   `dotemacs-nord'
;;   `dotemacs-nord-light'
;;   `dotemacs-opera'
;;   `dotemacs-opera-light'
;;   `dotemacs-outrun-electric'
;;   `dotemacs-nova'
;;   `dotemacs-palenight'
;;   `dotemacs-peacock'
;;   `dotemacs-snazzy'
;;   `dotemacs-solarized-dark'
;;   `dotemacs-solarized-light'
;;   `dotemacs-sourcerer'
;;   `dotemacs-spacegrey'
;;   `dotemacs-tomorrow-night'
;;   `dotemacs-tomorrow-day'
;;   `dotemacs-wilmersdorf'
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
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (dotemacs-themes-org-config)
;;
;;   ;; For treemacs users
;;   (dotemacs-themes-treemacs-config)
;;   ;; or for neotree users
;;   (dotemacs-themes-neotree-config)
;;
;;
;;; Code:

(require 'cl-lib)
(require 'dotemacs-themes-base)

(defgroup dotemacs-themes nil
  "Options for dotemacs-themes."
  :group 'faces)

(defcustom dotemacs-themes-padded-modeline nil
  "Default value for padded-modeline setting for themes that support it."
  :group 'dotemacs-themes
  :type '(choice integer boolean))

;;
(defcustom dotemacs-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'dotemacs-themes
  :type 'boolean)

(defcustom dotemacs-themes-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'dotemacs-themes
  :type 'boolean)

;;
;;; API

(defvar dotemacs-themes--colors nil)
(defvar dotemacs--min-colors '(257 256 16))
(defvar dotemacs--quoted-p nil)
(defvar dotemacs-themes--faces nil)

(defun dotemacs-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote dotemacs-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((dotemacs--quoted-p t))
                      (dotemacs-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (dotemacs--quoted-p)
                      (dotemacs-themes--colors-p (cdr item))))

                   ((or (dotemacs-themes--colors-p car)
                        (dotemacs-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not dotemacs--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item dotemacs-themes--colors))))))

(defun dotemacs-themes--apply-faces (new-faces &optional default-faces)
  (declare (pure t) (side-effect-free t))
  (let ((default-faces (or default-faces dotemacs-themes-base-faces))
        (faces (make-hash-table :test #'eq :size (+ (length default-faces) (length new-faces))))
        (directives (make-hash-table :test #'eq)))
    (dolist (spec (append (mapcar #'copy-sequence default-faces) new-faces))
      (if (listp (car spec))
          (cl-destructuring-bind (face action &optional arg) (car spec)
            (unless (assq face new-faces)
              (puthash face (list action arg (cdr spec))
                       directives)))
        (puthash (car spec) (cdr spec) faces)))
    (cl-loop for face being the hash-keys of directives
             for (action target spec) = (gethash face directives)
             unless (memq action '(&inherit &extend &override))
             do (error "Invalid operation (%s) for '%s' face" action face)
             if (eq (car spec) 'quote)
             do (error "Can't extend literal face spec (for '%s')" face)
             ;; TODO Add &all/&light/&dark extension support
             else if (memq (car spec) '(&all &light &dark))
             do (error "Can't extend face with &all, &light or &dark specs (for '%s')" face)
             else do
             (puthash face
                      (let ((old-spec (gethash (or target face) faces))
                            (plist spec))
                        ;; remove duplicates
                        (while (keywordp (car plist))
                          (setq old-spec (plist-put old-spec (car plist) (cadr plist))
                                plist (cddr plist)))
                        old-spec)
                      faces))
    (let (results)
      (maphash (lambda (face plist)
                 (when (keywordp (car plist))
                   ;; TODO Clean up duplicates in &all/&light/&dark blocks
                   (dolist (prop (append (unless dotemacs-themes-enable-bold   '(:weight normal :bold nil))
                                         (unless dotemacs-themes-enable-italic '(:slant normal :italic nil))))
                     (when (and (plist-member plist prop)
                                (not (eq (plist-get plist prop) 'inherit)))
                       (plist-put plist prop
                                  (if (memq prop '(:weight :slant))
                                      (quote 'normal))))))
                 (push (cons face plist) results))
               faces)
      (nreverse results))))

(defun dotemacs-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((dotemacs--quoted-p dotemacs--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote dotemacs-color))
                    item)
                   ((eq (car item) 'dotemacs-ref)
                    (dotemacs-themes--colorize
                     (apply #'dotemacs-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (dotemacs--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t dotemacs--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (dotemacs-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not dotemacs--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item dotemacs-themes--colors))
             `(dotemacs-color ',item ',type))

            (item)))))

(defun dotemacs-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (if (dotemacs-themes--colors-p real-attrs)
                    (dolist (cl dotemacs--min-colors `(list ,@(nreverse defs)))
                      (push `(list '((class color) (min-colors ,cl))
                                   (list ,@(dotemacs-themes--colorize real-attrs cl)))
                            defs))
                  `(list (list 't (list ,@real-attrs))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             ((let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((dotemacs-themes--colors-p real-attrs)
                                  (dolist (cl dotemacs--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(dotemacs-themes--colorize real-attrs cl)))
                                          defs)))

                                 ((push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))


;;
;;; Color helper functions

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

          (color1))))

;;;###autoload
(defun dotemacs-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (dotemacs-darken (dotemacs-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (dotemacs-darken c alpha)))

        ((dotemacs-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun dotemacs-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (dotemacs-lighten (dotemacs-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (dotemacs-lighten c alpha)))

        ((dotemacs-blend color "#FFFFFF" (- 1 alpha)))))

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


;;
;;; Defining themes

(defun dotemacs-themes-prepare-facelist (custom-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq dotemacs-themes--faces (dotemacs-themes--apply-faces custom-faces))
  (mapcar #'dotemacs-themes--build-face dotemacs-themes--faces))

(defun dotemacs-themes-prepare-varlist (vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (var val) in (append dotemacs-themes-base-vars vars)
           collect `(list ',var ,val)))

;;;###autoload
(defun dotemacs-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of dotemacs
theme face specs. These is a simplified spec. For example:

  (dotemacs-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(dotemacs-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(dotemacs-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(dotemacs-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(dotemacs-modeline-buffer-project-root :foreground green :weight 'bold))"
  (declare (indent defun))
  (apply #'custom-theme-set-faces
         (or theme 'user)
         (eval
          `(let* ((bold   ,dotemacs-themes-enable-bold)
                  (italic ,dotemacs-themes-enable-italic)
                  ,@(cl-loop for (var . val) in dotemacs-themes--colors
                             collect `(,var ',val)))
             (list ,@(mapcar #'dotemacs-themes--build-face faces))))))

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
       (unless bold (set-face-bold 'bold nil))
       (unless italic (set-face-italic 'italic nil))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))


(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
