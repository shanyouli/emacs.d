;;; bundles/dired/package.el -*- lexical-binding: t -*-

(package! dired-git-info :commands dired-git-info-mode)
(package! dired-quick-sort :commands hydra-dired-quick-sort/body
          :if (bundle-active-p 'hydra))
(package! dired-rsync :commands dired-rsync)
(package! diredfl :commands diredfl-global-mode)
(package! diff-hl :commands diff-hl-dired-mode)
(package! fd-dired :if (executable-find "fd"))
(package! dired-x :build-in t :commands (dired-omit-mode dired-x-find-file))
