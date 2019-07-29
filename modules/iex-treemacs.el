;;; iex-treemacs.el --- Initialize Treemacs -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (treemacs one-key)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Treemacs: A tree layout file explorer.

;;; Code:

;; install treemacs treemacs-projectile treemacs-magit -------------------------
(require-package 'treemacs)
(require-package 'treemacs-projectile)
(require-package 'treemacs-magit)
;;------------------------------------------------------------------------------

(require 'treemacs)

(setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
      treemacs-deferred-git-apply-delay      0.5
      treemacs-display-in-side-window        t
      treemacs-file-event-delay              5000
      treemacs-file-follow-delay             0.2
      treemacs-follow-after-init             t
      treemacs-git-command-pipe              ""
      treemacs-goto-tag-strategy             'refetch-index
      treemacs-indentation                   2
      treemacs-indentation-string            " "
      treemacs-is-never-other-window         nil
      treemacs-max-git-entries               5000
      treemacs-no-png-images                 nil
      treemacs-no-delete-other-windows       t
      treemacs-project-follow-cleanup        nil
      treemacs-recenter-distance             0.1
      treemacs-recenter-after-file-follow    nil
      treemacs-recenter-after-tag-follow     nil
      treemacs-recenter-after-project-jump   'always
      treemacs-recenter-after-project-expand 'on-distance
      treemacs-show-cursor                   nil
      treemacs-show-hidden-files             t
      treemacs-silent-filewatch              nil
      treemacs-silent-refresh                nil
      treemacs-sorting                       'alphabetic-desc
      treemacs-space-between-root-nodes      t
      treemacs-tag-follow-cleanup            t
      treemacs-tag-follow-delay              1.5
      treemacs-width                         30)

(with-eval-after-load 'treemacs
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;; Projectile integration for treemacs
(require 'treemacs-projectile)

;; Magit integration for treemacs
(require 'treemacs-magit)
(with-eval-after-load 'treemacs
  (with-eval-after-load 'magit
    (add-hook 'magit-post-commit-hook #'treemacs-magit--schedule-update)
    (add-hook 'git-commit-post-finish-hook #'treemacs-magit--schedule-update)
    (add-hook 'magit-post-stage-hook #'treemacs-magit--schedule-update)
    (add-hook 'magit-post-unstage-hook #'treemacs-magit--schedule-update)
    ))

;; treemacs icons
(if (locate-library "doom-themes")
    (require 'doom-themes-ext-treemacs))

;; treemacs-key
(defvar one-key-menu-treemacs-alist nil
  "The `one-key' menu alist for treemacs")
(setq one-key-menu-treemacs-alist
      '((("o" . "Open or close treemacs tree") . treemacs)
        (("s" . "Select window") . treemacs-select-window)
        (("k" . "Delete other-windows") . treemacs-delete-other-windows)
        (("f" . "Find file") . treemacs-find-file)
        (("t" . "Find Tags") . treemacs-find-tag)
        (("p" . "treemacs Projectile") . treemacs-projectile)))

(defun one-key-menu-treemacs ()
  "The `one-key' menu for TREEMACS."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('none (treemacs--init)))
  (one-key-menu "TREEMACS" one-key-menu-treemacs-alist t))

(provide 'iex-treemacs)

;;; iex-treemacs.el ends here
