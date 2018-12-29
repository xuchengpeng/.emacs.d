;;; feature/keybinds/config.el -*- lexical-binding: t; -*-

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)
  (which-key-mode +1))

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
    ("l" avy-goto-line :exit t))
  (defhydra hydra-mark (:color pink
                        :hint nil
                        :exit t)
    "
                    -- Mark --
"
    ("d" er/mark-defun "Defun / Function" :column "1-Symbol")
    ("f" er/mark-defun "Defun / Function")
    ("w" er/mark-word "Word")
    ("u" er/mark-url "Url")
    ("e" mark-sexp "S-Expression")
    ("E" er/mark-email "Email")
    ("p" er/mark-text-paragraph "Paragraph")
    ("s" er/mark-symbol "Symbol")
    ("S" er/mark-symbol-with-prefix "Prefixed symbol")
    ("q" er/mark-inside-quotes "Inside quotes" :column "2-Pairs")
    ("Q" er/mark-outside-quotes "Outside quotes")
    ("(" er/mark-inside-pairs "Inside pairs")
    ("[" er/mark-inside-pairs "Inside pairs")
    ("{" er/mark-inside-pairs "Inside pairs")
    (")" er/mark-outside-pairs "Outside pairs")
    ("]" er/mark-outside-pairs "Outside pairs")
    ("}" er/mark-outside-pairs "Outside pairs")
    ("t" er/mark-inner-tag "Inner tag")
    ("T" er/mark-outer-tag "Outer tag")
    ("c" er/mark-comment "Comment")
    ("a" er/mark-html-attribute "HTML attribute")
    ("." er/expand-region "Expand region" :exit nil :column "3-Region")
    ("," er/contract-region "Contract region" :exit nil)
    ("b" mark-whole-buffer "Buffer"))
  (defhydra hydra-workspaces (:color pink
                              :hint nil
                              :exit t)
    "               -- Workspaces --
"
    ("n" +workspace/new "New workspace" :column "1")
    ("l" +workspace/load "Load workspace from file")
    ("L" +workspace/load-session "Load a past session")
    ("s" +workspace/save "Save workspace to file")
    ("S" +workspace/save-session "Autosave current session")
    ("," +workspace/display "Display tab bar" :column "2")
    ("." +workspace/switch-to "Switch workspace")
    ("x" +workspace/kill-session "Delete session")
    ("d" +workspace/delete "Delete this workspace")
    ("r" +workspace/rename "Rename workspace")
    ("R" +workspace/load-last-session "Restore last session")
    ("q" nil "Quit" :column nil)))

(add-hook 'after-init-hook
          (lambda ()
            (if (featurep! :feature evil)
                (load! "+evil-bindings")
              (load! "+emacs-bindings"))))
