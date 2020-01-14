;;; bundles/dired/package.el -*- lexical-binding: t -*-

(package! 'dired-git-info :commands dired-git-info-mode)
(package! 'dired-quick-sort :commands hydra-dired-quick-sort/body
          :if (bundle-active-p 'hydra))
(package! 'dired-rsync :commands dired-rsync)
(package! 'diredfl :commands diredfl-global-mode)
(package! 'all-the-icons-dired
  :if (bundle-active-p 'icons)
  :commands all-the-icons-dired-mode)
(package! 'diff-hl :commands diff-hl-dired-mode)
(package! 'fd-dired :if (executable-find "fd"))
(package! 'dired-x :noinstall t :commands dired-omit-mode)
