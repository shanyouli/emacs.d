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

;; When I use Windows system, I hope emacs start-directory is "HOME" at emacs starting
(defun lye/default_dir_eq (list)
  "Make sure to open the directory for HOME on windows system!"
  (if (string-equal default-directory (car list))
      (cd "~/")
    (unless (not list)
      (lye/default_dir_eq (cdr list)))))

(add-hook 'after-init-hook
          (lambda ()
            (let ((dir_list
                   '(
                     "c:/Applications/ScoopApps/apps/emacs/current/bin/"
                     "c:/Applications/ScoopApps/apps/emacs-dev/current/bin/"
                     "c:/emacs/bin/"
                     "c:/Applications/emacs/bin/"
                     "d:/ScoopApps/apps/emacs-dev/current/bin/"
                     "d:/ScoopApps/apps/emacs/current/bin/"
                     )))
              (lye/default_dir_eq dir_list))))

;; confirmedto prevent misoperation.
(setq comfirm-kill-emacs (lambda (prompt)
                           (y-or-n-p-with-timeout "Exit Emacs after 3s?" 3 "y")))

;; autohotkey-mode
(use-package ahk-mode :mode "\\.ahk\\'"  :defer t)

;; Runninng Wsl in Emacs
;; see @https://github.com/MatthewZMD/.emacs.d#bash-command
(defun lye/wsl-bash ()
  "Running windows subsystem Linux in emacs!"
  (interactive)
  (let ((shell-file-name "C:\\Windows\\System32\\bash.exe"))
    (shell "*bash*")))

(provide 'init-ahk)
;;; init-ahk.el ends here
