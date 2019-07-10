;;; init-basic.el --- Initialize basic configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic Configuration.

;;; Code:

;; Set name and mail-address
(setq user-full-name lye-full-name)
(setq user-mail-address lye-mail-address)

;; Use real line movement instead of visual line movement
(setq track-eol t) ; keep cursor at end of lines
(setq line-move-visual nil)
;; @see https://emacs-china.org/t/spacemacs/9000
(setq auto-save-list-file-prefix nil ;not.# and #.# file
      auto-save-default nil ; not auto-save file
      make-backup-files nil ; not ~ file
      create-lockfiles nil) ; not .#*** file
;; Store all temporal files in a temporal directory instead of being
;; disseminated in the $HOME directory
(setq-default
  ;; Tramp history
 tramp-persistency-file-name (concat lye-emacs-cache-dir "tramp")
 ;; Bookmark-default-file
 bookmark-default-file (concat lye-emacs-cache-dir "bookmarks")
 ;; SemanticDB files
 semanticdb-default-save-directory (concat lye-emacs-cache-dir "semanticdb")
 ;; url files
 url-configuration-directory (concat lye-emacs-cache-dir "url")
 ;; eshell files
 eshell-directory-name (concat lye-emacs-cache-dir "eshell")
;; Game score
 gamegrid-user-score-file-directory (concat lye-emacs-cache-dir "games"))
; Don't compact font caches during GC.
(setq inhibit-compacting-font-caches t)
;; y/n replace yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;;不要烦人的 redefine warning
(setq ad-redefinition-action 'accept)
;; Turn off the error ringtone
(setq ring-bell-function 'ignore)
;; Paste at the cursor position instead of the mouse pointer
(setq mouse-yank-at-point t)
;; 支持 emacs 和外部程序的粘贴
(setq x-select-enable-clipboard t)

;;; some package initalize

;; Start server
;; @see https://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
(use-package server
  :if (not (or (eq system-type 'cygwin) system/windows))
  :ensure nil
  :commands (server-running-p)
  :hook (after-init . (lambda () (unless (server-running-p) (server-start))))
  :init (setq server-auth-dir (concat lye-emacs-cache-dir "server")))

;; Save cursor position for everyfile you opened. So,  next time you open
;; the file, the cursor will be at the position you last opened it.
(use-package saveplace
  :ensure nil
  :config (setq save-place-file (concat lye-emacs-cache-dir "saveplace"))
  :hook (after-init . save-place-mode))

;; Miantain a history of past actions and a resonable number of lists
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (progn
    (setq-default history-length 1000)
    (setq savehist-file (concat lye-emacs-cache-dir "history")
          enable-recursive-minibuffers t
          history-delete-duplicates t
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 60)))

;; Save recentf file and open them
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-save-file (concat lye-emacs-cache-dir "recentf"))
  ;;Do not add these files to the recently opened text
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          "bookmarks"
                          "ido.*"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'"
                          "COMMIT_MSG")))

;; Use undo-tree
(use-package undo-tree
  :ensure nil
  :commands global-undo-tree-mode
  :hook (after-init . global-undo-tree-mode))
;; (setq undo-tree-history-directory-alist
;;       `(("." . ,(concat lye-emacs-cache-dir "undo"))))

;; Save Emacs buffers when they lose focus after 2s
(use-package auto-save
  :ensure nil
  :defines (auto-save-silent auto-save-idle)
  :commands (auto-save-enable)
  :init (setq auto-save-silent t
              auto-save-idle 2)
  :hook (after-init . auto-save-enable))

;; Displays the key bindings following your currently entered incomplete command
(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode))

;; restart emacs
(use-package restart-emacs
  :ensure nil
  :commands (restart-emacs))

;; Esup,Start time adjustment<Emacs Start Up Profiler>
(use-package esup :ensure nil :commands esup)

(require 'noflet)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (noflet ((process-list ())) ad-do-it))
;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


(provide 'init-basic)
;;; init-basic.el ends here
