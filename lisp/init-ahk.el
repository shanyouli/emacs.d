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

;; confirmedto prevent misoperation.
(setq confirm-kill-emacs
      (lambda (prompt)
        (y-or-n-p-with-timeout "Exit Emacs after 1.5s?" 1.5 "y")))

(cd "~")

;; Return the button at the beginning of win other than Win+L to Emacs
;; see @https://emacs-china.org/t/superkey/9387
(w32-register-hot-key [s-])

(provide 'init-ahk)
;;; init-ahk.el ends here
