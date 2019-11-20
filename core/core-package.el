;;; core-package.el --- Initialize third-packages -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
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

;;; Code:

;; Start server
;; @see https://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
(when (and (display-graphic-p) (not IS-WINDOWS))
  (add-hook 'lye-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

;; Save cursor position for everyfile you opened. So,  next time you open
;; the file, the cursor will be at the position you last opened it.
(add-hook 'lye-init-hook
          (lambda ()
            (require 'saveplace)
            (save-place-mode +1)))

;; Miantain a history of past actions and a resonable number of lists
(add-hook 'lye-init-hook
          (lambda ()
            (require 'savehist)
            (setq-default history-length 1000)
            (setq enable-recursive-minibuffers t
                  history-delete-duplicates t
                  savehist-additional-variables '(mark-ring
                                                  global-mark-ring
                                                  search-ring
                                                  regexp-search-ring
                                                  extended-command-history)
                  savehist-autosave-interval 60)
            (savehist-mode +1)))

;; Save recentf file and open them
(add-hook 'lye-init-hook
          (lambda ()
            (require 'recentf)

            (setq recentf-max-saved-items 200
                  ;;Do not add these files to the recently opened text
                  recentf-exclude '((expand-file-name package-user-dir)
                                    ".cache"
                                    ".cask"
                                    "bookmarks"
                                    "ido.*"
                                    "recentf"
                                    "url"
                                    "COMMIT_EDITMSG\\'"
                                    "COMMIT_MSG"
                                    ))
            (recentf-mode)))

;; Automatically refresh files that have been changed elsewhere
(add-hook 'lye-init-hook (lambda () (global-auto-revert-mode +1)))

(add-hook 'lye-init-hook
          (lambda ()
            ;; Use undo-tree
            (global-undo-tree-mode +1)

            ;; Save Emacs buffers when they lose focus after 1.5s
            (setq md-save-silent-p t
                  md-save-idle-duraion 1.5)
            (md-save-global-mode)
            ;; Displays the key bindings following your currently entered
            ;; incomplete command
            (setq which-key-idle-delay 0.5
                  which-key-popup-type 'minibuffer)
            (which-key-mode +1)

          ;; not use mouse
            (if (display-graphic-p) (global-disable-mouse-mode))))

;;; Backup-file
(unless IS-WINDOWS
  (add-hook 'lye-init-hook
            (lambda ()
              (package! '(backup-file :type git :host github
                                      :repo "shanyouli/emacs-backup-file"))
              (setq backup-file-location (expand-file-name "backup"
                                                         lye-emacs-cache-dir))
              (add-hook 'after-save-hook #'backup-file)
              (mdk/set-key! "C-z s b"  'backup-file-log))))

;;; Highlight diff
(use-package diff-hl
  :ensure t
  :commands (global-diff-hl-mode)
  :hook (after-init . global-diff-hl-mode))

(lye/tools-module-install "winum")

(if (executable-find "sdcv")
    (lye/apps-module-install "sdcv" t)
  (lye/apps-module-install "ydcv" t))

(provide 'core-package)

;;; core-package.el ends here
