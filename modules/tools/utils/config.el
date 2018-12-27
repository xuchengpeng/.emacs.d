;;; tools/utils/config.el -*- lexical-binding: t; -*-

(if EMACS26+
    (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (use-package nlinum
    :hook ((prog-mode text-mode conf-mode) . nlinum-mode)
    :config
    (setq nlinum-format "%4d"))
  (use-package nlinum-relative
    :commands (nlinum-relative-mode
               nlinum-relative-toggle
               nlinum-relative-on
               nlinum-relative-off)
    :config
    (setq nlinum-relative-current-symbol ""
          nlinum-relative-redisplay-delay 0)
    (when (featurep! :feature evil)
      (nlinum-relative-setup-evil))))

(use-package hydra
  :defer t
  :init
  (defhydra hydra-flycheck (:color pink)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^--------^----------^------^------------^-------^----------
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
    ("q" nil)
    ("<" flycheck-previous-error)
    (">" flycheck-next-error)
    ("?" flycheck-describe-checker :color blue)
    ("d" flycheck-disable-checker :color blue)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors :color blue)
    ("m" flycheck-manual :color blue)
    ("s" flycheck-select-checker :color blue)
    ("v" flycheck-verify-setup :color blue))
  (defhydra hydra-toggle (:color pink
                          :hint nil)
    "
_a_: abbrev-mode
_d_: debug-on-error
_f_: auto-fill-mode
_o_: org-mode
_t_: truncate-lines
_w_: whitespace-mode
_q_: Quit

    "
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("o" org-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil))
  (defhydra hydra-window (:color pink
                          :hint nil)
    "
               -- WINDOW MENU --
^^^^^^^^-----------------------------------------------------
^Move Point^     ^Move Splitter^  ^Split^
^^^^^^^^-----------------------------------------------------
_<up>_           _<S-up>_         _0_: delete-window
_<left>_         _<S-left>_       _1_: delete-other-windows
_<down>_         _<S-down>_       _2_: split-window-below
_<right>_        _<S-right>_      _3_: split-window-right
You can use arrow-keys or WASD.
_q_: Quit
    "
    ("0" delete-window :exit t)
    ("1" delete-other-windows :exit t)
    ("2" split-window-below :exit t)
    ("3" split-window-right :exit t)
    ("a" windmove-left nil)
    ("s" windmove-down nil)
    ("w" windmove-up nil)
    ("d" windmove-right nil)
    ("A" hydra-move-splitter-left nil)
    ("S" hydra-move-splitter-down nil)
    ("W" hydra-move-splitter-up nil)
    ("D" hydra-move-splitter-right nil)
    ("<left>" windmove-left nil)
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<right>" windmove-right nil)
    ("<S-left>" hydra-move-splitter-left nil)
    ("<S-down>" hydra-move-splitter-down nil)
    ("<S-up>" hydra-move-splitter-up nil)
    ("<S-right>" hydra-move-splitter-right nil)
    ("u" hydra--universal-argument nil)
    ("q" nil))
  (defhydra hydra-multiple-cursors (:color pink
                                    :hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Prev    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  (defhydra hydra-avy (:hint nil)
    "
     ^Chars^            ^other^
-------------------------------------
[_c_]   char         [_w_]   word
[_C_]   char-2       [_s_]   subword
[_t_]   char-timer   [_l_]   line
"
    ("c" avy-goto-char :exit t)
    ("C" avy-goto-char-2 :exit t)
    ("t" avy-goto-char-timer :exit t)
    ("w" avy-goto-word-1 :exit t)
    ("s" avy-goto-subword-1 :exit t)
    ("l" avy-goto-line :exit t)))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize)))
