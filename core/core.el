;;; core.el --- Initialize core configurations. -*- lexical-binding: t; -*-

(eval-when-compile
  (when (version< emacs-version "26.1")
    (error "Detected Emacs %s. Emacs version should be 26.1 or higher"
           emacs-version)))

;; Ensure `dotemacs-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

;;
;;; Global variables

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar dotemacs-debug-p (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all dotemacs functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defvar dotemacs-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defvar dotemacs-init-time nil
  "The time it took, in seconds, for Emacs to initialize.")

(defconst dotemacs-dir user-emacs-directory
  "The path to this emacs.d directory.")

(defconst dotemacs-core-dir (concat dotemacs-dir "core/")
  "Where core files are stored.")

(defconst dotemacs-modules-dir (concat dotemacs-dir "modules/")
  "Where modules files are stored.")

(defconst dotemacs-local-dir (concat dotemacs-dir ".local/")
  "Root directory for local Emacs files.")

(defconst dotemacs-cache-dir (concat dotemacs-dir ".cache/")
  "Where cache files are stored.")

(defconst dotemacs-packages-dir (concat dotemacs-local-dir "packages/")
  "Where packages are stored.")

(defconst dotemacs-private-dir (concat dotemacs-local-dir "private/")
  "Where your private customizations are placed. Must end in a slash.")

(defconst dotemacs-autoload-file (concat dotemacs-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")

(defgroup dotemacs nil
  "dotemacs, an Emacs configuration."
  :group 'emacs)

;;
;;; Custom error types

(define-error 'dotemacs-error "Error in dotemacs Emacs core")
(define-error 'dotemacs-hook-error "Error in a dotemacs startup hook" 'dotemacs-error)
(define-error 'dotemacs-autoload-error "Error in an autoloads file" 'dotemacs-error)
(define-error 'dotemacs-module-error "Error in a dotemacs module" 'dotemacs-error)
(define-error 'dotemacs-private-error "Error in private config" 'dotemacs-error)
(define-error 'dotemacs-package-error "Error with packages" 'dotemacs-error)

;;
;;; Custom hooks

(defvar dotemacs-first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'dotemacs-first-input-hook 'permanent-local t)

(defvar dotemacs-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'dotemacs-first-file-hook 'permanent-local t)

(defvar dotemacs-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'dotemacs-first-buffer-hook 'permanent-local t)

;;
;;; Emacs core configuration

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error dotemacs-debug-p
      jka-compr-verbose dotemacs-debug-p)

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but the clipboard's on Windows could be in another encoding (likely
;; utf-16), so let Emacs/the OS decide what to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream
(setq ad-redefinition-action 'accept)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error dotemacs-debug-p
      jka-compr-verbose dotemacs-debug-p)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1.0)

(setq abbrev-file-name             (concat dotemacs-local-dir "abbrev.el")
      async-byte-compile-log-file  (concat dotemacs-cache-dir "async-bytecomp.log")
      bookmark-default-file        (concat dotemacs-cache-dir "bookmarks")
      pcache-directory             (concat dotemacs-cache-dir "pcache/")
      request-storage-directory    (concat dotemacs-cache-dir "request")
      shared-game-score-directory  (concat dotemacs-cache-dir "shared-game-score/")
      tramp-auto-save-directory    (concat dotemacs-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat dotemacs-cache-dir "tramp-persistency.el")
      url-cache-directory          (concat dotemacs-cache-dir "url/")
      url-configuration-directory  (concat dotemacs-cache-dir "url/"))

(unless custom-file
  (setq custom-file (concat dotemacs-local-dir "custom.el")))
(load custom-file t (not dotemacs-debug-p))

;;
;;; Optimizations

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(setq gcmh-idle-delay 5
      gcmh-verbose dotemacs-debug-p)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased by our more expensive LSP module, and where needed.
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help with performance while scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;;
;;; Bootstrap helpers

(defun dotemacs-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (dotemacs-log "Running dotemacs hook: %s" hook)
  (condition-case-unless-debug e
      (funcall hook)
    (user-error
     (warn "Warning: %s" (error-message-string e)))
    (error
     (signal 'dotemacs-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun dotemacs-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (run-hook-wrapped hook #'dotemacs-run-hook)
      (dotemacs-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s" (cadr e) (caddr e)))
       (signal 'dotemacs-hook-error (cons hook (cdr e)))))))

(defun dotemacs-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (or (daemonp)
                           (and (boundp hook)
                                (symbol-value hook))))
              (dotemacs-run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn (if EMACS27+ -101))))
      fn)))

;; benchmark
(defun dotemacs-display-benchmark-h (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Emacs loaded %s packages across %d modules in %.03fs"
           (+ (length dotemacs-core-packages) (length dotemacs-packages))
           (if dotemacs-modules (hash-table-count dotemacs-modules) 0)
           (or dotemacs-init-time
               (setq dotemacs-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

(defun dotemacs-initialize-core ()
  "Initialize core"
  ;; Make sure all essential local directories exist
  (dolist (dir (list dotemacs-local-dir dotemacs-cache-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  (require 'core-ui)
  (require 'core-editor)
  (require 'core-keybinds)
  (require 'core-modules))

(defun dotemacs-initialize ()
  "dotemacs initialize function.
The load order is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  Module packages.el files
  Module init.el files
  `dotemacs-before-init-modules-hook'
  Module config.el files
  `dotemacs-init-modules-hook'
  `after-init-hook'
  `emacs-startup-hook'
  `window-setup-hook'

Module load order is determined by your `dotemacs!' block."
  (add-hook 'window-setup-hook #'dotemacs-display-benchmark-h)
  (dotemacs-run-hook-on 'dotemacs-first-buffer-hook '(find-file-hook dotemacs-switch-buffer-hook))
  (dotemacs-run-hook-on 'dotemacs-first-file-hook   '(find-file-hook dired-initial-position-hook))
  (dotemacs-run-hook-on 'dotemacs-first-input-hook  '(pre-command-hook))

  (dotemacs-initialize-modules))

;;
;;; Initialize dotemacs core

(dotemacs-initialize-core)

(provide 'core)
;;; core.el ends here
