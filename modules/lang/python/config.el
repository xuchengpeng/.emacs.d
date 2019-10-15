;;; lang/python/config.el -*- lexical-binding: t; -*-

(use-package python
  :defer t
  :init
  (setq python-environment-directory dotemacs-cache-dir
        python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  
  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))
  
  (setq-hook! 'python-mode-hook tab-width python-indent-offset))

(use-package pyimport
  :after python
  :config
  (map-local! python-mode-map
    "i"  '(:ignore t :which-key "imports")
    "ii" '(pyimport-insert-missing :which-key "Insert missing imports")
    "ir" '(pyimport-remove-unused :which-key "Remove unused imports")
    "is" '(pyimpsort-buffer :which-key "Sort imports")
    "io" '(+python/optimize-imports :which-key "Optimize imports")))

(use-package python-pytest
  :defer t
  :init
  (map-local! python-mode-map
    "t"  '(:ignore t :which-key "test")
    "tf" 'python-pytest-file-dwim
    "tF" 'python-pytest-file
    "tt" 'python-pytest-function-dwim
    "tT" 'python-pytest-function
    "tr" 'python-pytest-repeat
    "tp" 'python-pytest-popup))
