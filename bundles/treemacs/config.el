;;; bundles/treemeacs/config.el.el -*- lexical-binding: t -*-

(with-eval-after-load 'treemacs
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
      (treemacs-git-mode 'deferred))
    (`(t . _)
      (treemacs-git-mode 'simple)))

  (with-eval-after-load 'magit
    (dolist (h (list magit-post-commit
                     git-commit-post-finish
                     magit-post-stage
                     magit-post-unstage))
      (add-hook! 'h (treemacs-magit--schedule-update)))))
