;;; dotemacs-packages.el --- Initialize dotemacs packages configurations. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Package configuration.
;;

;;; Code:

(setq package-enable-at-startup nil
      package-user-dir (concat dotemacs-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir))

(setq straight-base-dir (file-truename dotemacs-local-dir)
      straight-repository-branch "master"
      straight-vc-git-default-clone-depth '(1 single-branch))

(defun dotemacs-ensure-straight ()
  (let ((repo-dir (concat straight-base-dir "straight/repos/straight.el"))
        (repo-url "https://github.com/radian-software/straight.el.git"))
    (unless (file-directory-p repo-dir)
      (unless (executable-find "git")
        (message "[dotemacs] Git isn't present on your system. Cannot proceed."))
      (dotemacs-call-process "git" "clone" repo-url repo-dir
                             "--depth" "1" "--no-tags"
                             "--branch" straight-repository-branch))
    (require 'straight (concat repo-dir "/straight.el"))
    (mapc #'straight-use-recipes
          '((org-elpa :local-repo nil)
            (melpa              :type git :host github
                                :repo "melpa/melpa"
                                :build nil)
            (gnu-elpa-mirror    :type git :host github
                                :repo "emacs-straight/gnu-elpa-mirror"
                                :build nil)
            (el-get             :type git :host github
                                :repo "dimitri/el-get"
                                :build nil)
            (emacsmirror-mirror :type git :host github
                                :repo "emacs-straight/emacsmirror-mirror"
                                :build nil)))))

(dotemacs-ensure-straight)
(straight-use-package
 `(straight :host github
            :repo "radian-software/straight.el"
            :files ("straight*.el")
            :branch ,straight-repository-branch))

(defmacro dotemacs-require-package (package)
  `(straight-use-package ,package))

(defmacro dotemacs-require-packages (packages-list)
  `(mapc #'straight-use-package ,packages-list))

(dotemacs-require-packages '(use-package bind-key))

(provide 'dotemacs-packages)
;;; dotemacs-packages.el ends here
