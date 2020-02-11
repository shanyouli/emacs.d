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

(add-hook! 'after-init-hook
  :defer 0.1
  :append t
  ;; Start server
  ;; @see https://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
  (when (and (display-graphic-p) (not IS-WINDOWS))
    (autoload 'server-running-p "server")
    (unless (server-running-p) (server-start)))

  ;; Save cursor position for everyfile you opened. So,  next time you open
  ;; the file, the cursor will be at the position you last opened it.
  (autoload 'save-place-mode "saveplace")
  (save-place-mode +1)

  ;; Miantain a history of past actions and a resonable number of lists
  (autoload 'savehist-mode "savehist")
  (setq-default history-length 1000)
  (setq enable-recursive-minibuffers t
        history-delete-duplicates t
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode +1)

  ;; Save recentf file and open them
  (autoload 'recentf-mode "recentf")
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
                          "\\/sudo:root\\@localhost:*"
                          "\\/sudo:root\\@*"))
  (recentf-mode +1)

  ;; Automatically refresh files that have been changed elsewhere
  (global-auto-revert-mode +1)

  ;; Use undo-tree
  ;;(global-undo-tree-mode +1)

  ;; Save Emacs buffers when they lose focus after 2s
  (with-eval-after-load 'super-save
      (push 'split-window-below super-save-triggers)
      (push 'split-window-right super-save-triggers))
  (super-save-mode +1)
  ;; Displays the key bindings following your currently entered
  ;; incomplete command
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  (which-key-mode +1)

  ;; not use mouse
  (when (display-graphic-p) (global-disable-mouse-mode +1))

  ;; Backup-file
  (unless IS-WINDOWS
    (package+ '(backup-file :type git :host github
                :repo "shanyouli/emacs-backup-file"))
    (setq backup-file-location (expand-file-name "backup"
                                                 lye-emacs-cache-dir))
    (add-hook! 'after-save-hook 'backup-file)
    (lib-key-define "C-z s b" 'backup-file-log :autoload "backup-file"))

  ;; Highlight diff
  (autoload 'global-diff-hl-mode "diff-hl")
  (global-diff-hl-mode +1))

(provide 'core-package)

;;; core-package.el ends here
