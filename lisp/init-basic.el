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
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(setq user-full-name lye-full-name)
(setq user-mail-address lye-mail-address)
;; Set the temporal directory
(unless (file-exists-p lye-emacs-temporal-dir)
  (make-directory lye-emacs-temporal-dir))
;; Store all temporal files in a temporal directory instead of being
;; disseminated in the $HOME directory
(setq-default
 ;; Tramp history
 tramp-persistency-file-name (concat lye-emacs-temporal-dir "tramp")
 ;; Bookmark-default-file
 bookmark-default-file (concat lye-emacs-temporal-dir "bookmarks")
 ;; SemanticDB files
 semanticdb-default-save-directory (concat lye-emacs-temporal-dir "semanticdb")
 ;; url files
 url-configuration-directory (concat lye-emacs-temporal-dir "url")
 ;; eshell files
 eshell-directory-name (concat lye-emacs-temporal-dir "eshell")
;; Game score
 gamegrid-user-score-file-directory (concat lye-emacs-temporal-dir "games")
 )


;; Start server
;; @see https://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
(unless (eq system-type 'cygwin)
  (use-package server
    :ensure nil
    :commands (server-running-p)
    :hook (after-init . (lambda ()
                          (unless (server-running-p)
                            (server-start))))
    :init (setq server-auth-dir (concat lye-emacs-temporal-dir "server"))))

;; Save cursor position for everyfile you opened. So,  next time you open
;; the file, the cursor will be at the position you last opened it.
(use-package saveplace
  :ensure nil
  :config
  (progn
    (setq save-place-file (concat lye-emacs-temporal-dir "saveplace")))
  :hook (after-init . save-place-mode))

;; Miantain a history of past actions and a resonable number of lists
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (progn
    (setq-default history-length 1000)
    (setq savehist-file (concat lye-emacs-temporal-dir "history")
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
  :hook (find-file . (lambda () (unless recentf-mode
                             (recentf-mode)
                             (recentf-track-opened-file))))
  :init
  ;;(add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200
        recentf-save-file (concat lye-emacs-temporal-dir "recentf"))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "COMMIT_MSG"))

;; Delete useless spaces before saving the file
(use-package files
  :ensure nil
  :config
  ;; When Emacs is closed under the Window system, it is forcibly
  ;; confirmedto prevent misoperation.
  ;; @see https://lujun9972.github.io/blog/2017/04/15/%E9%98%B2%E6%AD%A2%E6%84%8F%E5%A4%96%E9%80%80%E5%87%BAemacs/
  (when *is-a-win*
    (setq confirm-kill-emacs (lambda (prompt)
                               (y-or-n-p-with-timeout "Exit Emacs after 3s?" 3 "y")))))

;; (use-package simple
;;   :ensure nil
;;   :hook (before-save . (lambda ()
;;                          (delete-trailing-whitespace))))

(setq auto-save-list-file-prefix nil;not.# and #.# file
      auto-save-default nil
      make-backup-files nil) ; not ~ file

;; Save Emacs buffers when they lose focus after 1s
(use-package auto-save
  :straight (auto-save :type git :host github :repo "manateelazycat/auto-save")
  :ensure nil
  :commands (auto-save-enable)
  :hook (after-init . auto-save-enable)
  :config
  (setq auto-save-idle 2)
  (setq auto-save-silent t))

;;Displays the key bindings following your currently entered incomplete command
(use-package which-key
  :diminish which-key
  :hook (after-init . which-key-mode))

;; When I use Windows system, I hope emacs start-directory is "HOME" at emacs starting
(when *is-a-win*
  (if (string-equal default-directory "c:/Applications/ScoopApps/apps/emacs/current/bin/")
      (cd "~")))

(provide 'init-basic)
;;; init-basic.el ends here
