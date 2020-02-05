;;; bundles/treemeacs/package.el.el -*- lexical-binding: t -*-


(package! 'treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode
             treemacs
             treemacs-select-window
             treemacs-delete-other-windows
             treemacs-bookmark
             treemacs-find-file
             treemacs-find-tag))

(package! 'treemacs-projectile :commands treemacs-projectile)

(package! 'treemacs-magit :commands treemacs-magit--schedule-update)
