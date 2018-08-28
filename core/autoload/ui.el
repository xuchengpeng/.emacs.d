;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;; font configurations
;; Solution 1

;; (defun dotemacs-font-existsp (font)
;;   "Detect if a font exists"
;;   (if (null (x-list-fonts font))
;;       nil
;;     t))
;; or
;; (defun dotemacs-font-existsp (font)
;;   "Detect if a font exists"
;;   (if (find-font (font-spec :family font))
;;         t
;;       nil))
;; 
;; (defun dotemacs-make-font-string (font-name font-size)
;;   "Make font string with FONT-NAME and FONT-SIZE."
;;   (if (and (stringp font-size)
;;            (equal ":" (string (elt font-size 0))))
;;       (format "%s%s" font-name font-size)
;;     (format "%s %s" font-name font-size)))
;; 
;; (defun dotemacs-set-english-chinese-font (english-fonts
;;                                           english-font-size
;;                                           chinese-fonts
;;                                           &optional chinese-font-size chinese-fonts-scale)
;;   "Set english(with ENGLISH-FONTS and ENGLISH-FONT-SIZE) and chinese(with CHINESE-FONTS,
;; CHINESE-FONT-SIZE and CHINESE-FONTS-SCALE) font."
;;   (setq chinese-font-size (or chinese-font-size 16)
;;         chinese-fonts-scale (or chinese-fonts-scale 1.2))
;; 
;;   "english-font-size could be set to \":pixelsize=18\" or a integer.
;;    If set/leave chinese-font-size to nil, it will follow english-font-size"
;;   (require 'cl) ; for find if
;;   (let ((en-font (dotemacs-make-font-string
;;                   (find-if #'dotemacs-font-existsp english-fonts)
;;                   english-font-size))
;;         (zh-font (font-spec :family (find-if #'dotemacs-font-existsp chinese-fonts)
;;                             :size chinese-font-size)))
;; 
;;     ;; Set English font
;;     ;; (message "Set English Font to %s" en-font)
;;     (set-face-attribute 'default nil :font en-font)
;; 
;;     ;; Set Chinese font
;;     ;; (message "Set Chinese Font to %s" zh-font)
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset zh-font)))
;;     
;;     ;; Fix chinese font width and rescale
;;     (setq face-font-rescale-alist '(("STHeiti" . ,chinese-fonts-scale)
;;                                     ("STFangsong" . ,chinese-fonts-scale)
;;                                     ("Microsoft Yahei" . ,chinese-fonts-scale))))
;; 
;; ;;;###autoload
;; (defun dotemacs-set-font()
;;   "Set fonts."
;;   (dotemacs-set-english-chinese-font
;;     '("DejaVu Sans Mono" "Source Code Pro" "Monaco" "Consolas") 11
;;     '("STXihei" "STHeiti" "STFangsong" "STZhongsong" "Microsoft Yahei" "黑体" "新宋体" "宋体") 16))

;; Solution 2

;;;###autoload
(defun dotemacs-set-font()
  "Set english and chinese fonts."
  (setq fonts
        (cond ((eq system-type 'darwin)
               '("Monaco" "STHeiti"))
              ((eq system-type 'gnu/linux)
               '("DejaVu Sans Mono" "STHeiti"))
              ((memq system-type '(cygwin windows-nt ms-dos))
               '("Consolas" "Microsoft Yahei"))))
  
  (let* ((en-font      (or dotemacs-font (car fonts)))
         (cn-font      (or dotemacs-cn-font (car (cdr fonts))))
         (en-font-size (or dotemacs-font-size 11))
         (cn-font-size (or dotemacs-cn-font-size 16)))
    (set-face-attribute 'default nil :font
                        (format "%s %d" en-font en-font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family cn-font :size cn-font-size)))
    ;; Fix chinese font width and rescale
    (setq face-font-rescale-alist '((cn-font . 1.2)))))

;; Solution 3
;; 
;; ;;;###autoload
;; (defun dotemacs-set-font()
;;   "Set english and chinese fonts."
;;   ;; Setting English Font
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (set-face-attribute 'default nil :font
;;                         (format "%s:pixelsize=%d" "DejaVu Sans Mono" 14)))
;;   
;;   ;; Setting Chinese font
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset
;;                       (font-spec :family "STHeiti" :size 16)))
;;   
;;   ;; Fix chinese font width and rescale
;;   (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2))))

