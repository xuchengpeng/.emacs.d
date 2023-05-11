;;; dotemacs-packages.el --- Initialize dotemacs packages configurations.

(setq package-enable-at-startup nil
      package-user-dir (concat dotemacs-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir))

(setq straight-base-dir (file-truename dotemacs-local-dir)
      straight-repository-branch "master"
      straight-vc-git-default-clone-depth '(1 single-branch))

(defun dotemacs-ensure-straight ()
  (let ((repo-dir (expand-file-name "straight/repos/straight.el" straight-base-dir))
        (repo-url "https://github.com/radian-software/straight.el.git")
        (call (lambda (&rest args)
                (apply #'dotemacs-call-process args))))
    (unless (file-directory-p repo-dir)
      (unless (executable-find "git")
        (message "[dotemacs] Git isn't present on your system. Cannot proceed."))
      (funcall call "git" "clone" repo-url repo-dir
                    "--depth 1"
                    "--no-tags"
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

(defun dotemacs-require-package (package)
  (straight-use-package package))

(defun dotemacs-require-packages (packages-list)
  (mapc #'dotemacs-require-package packages-list))

(defvar dotemacs-core-packages '(use-package bind-key general)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(dotemacs-require-packages dotemacs-core-packages)

(provide 'dotemacs-packages)
;;; dotemacs-packages.el ends here