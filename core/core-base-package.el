;;; core-base-package.el --- Initialize base package -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: keywords


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

;; base package

;;; Code:

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

;;Extra blank hint
(use-package whitespace
  :ensure nil
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

;; Automatically refresh files that have been changed elsewhere
(add-hook 'after-init-hook (lambda () (global-auto-revert-mode t)))

(provide 'core-base-package)

;;; core-base-package.el ends here
