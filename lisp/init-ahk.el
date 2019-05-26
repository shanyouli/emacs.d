;;; init-ahk.el ---AutoHotKey Languages Configurations Initialize.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords: languages

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

;; AHK Configurations

;;; Code:

;; autohotkey-mode
(use-package ahk-mode :mode "\\.ahk\\'"  :defer t)

;; powershell-mode
(use-package powershell :mode ("\\.ps1\\'" . powershell-mode) :defer t)

;; Runninng Wsl in Emacs
;; see @https://github.com/MatthewZMD/.emacs.d#bash-command
(defun lye/wsl-bash ()
  "Running windows subsystem Linux in emacs!"
  (interactive)
  (let ((shell-file-name "C:\\Windows\\System32\\bash.exe"))
    (shell "*bash*")))

;; Us ido-mode
(use-package ido
  :ensure nil
  :bind (("C-x C-f" . ido-find-file)
         ("C-x C-r" . ido-recentf-open))
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  ;; Guess if the cursor position is an openable directory or file
  (setq ido-use-filename-at-point 'guess)
  ;;(setq ido-use-url-at-point)
  ;; Ido does not automatically create a new buffer, need to ask
  (setq ido-create-new-buffer 'prompt)
  (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  (setq ido-save-directory-list-file (concat lye-emacs-temporal-dir "ido.last"))

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  (ido-mode 1))

;; use smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands))
  :config
  (setq smex-save-file  (concat lye-emacs-temporal-dir "smex-items"))
  (setq smex-history-length 10)
  (smex-initialize))

;; confirmedto prevent misoperation.
(setq confirm-kill-emacs
      (lambda (prompt)
        (y-or-n-p-with-timeout "Exit Emacs after 1.5s?" 1.5 "y")))


(setq default-directory "~")

;; Return the button at the beginning of win other than Win+L to Emacs
;; see @https://emacs-china.org/t/superkey/9387
(w32-register-hot-key [s-])

(provide 'init-ahk)
;;; init-ahk.el ends here
