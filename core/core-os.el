;;; core-os.el --- initialize PATH -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (exec-path-from-shell cache-path-from-shell)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: exec-path


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

;; Emacs executor environment variable configuration

;;; Code:

;; Mac, X, GNU-step
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (require 'cache-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  ;; (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (setq exec-path-from-shell-arguments '("-l")))

(defun lye/exec-path-from-shell-init ()
  "Avoid importing `exec-path-from-shell' on some operating systems."
  (if (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

(when system/windows
  ;;;Msys2 configuration
  (defvar msys2-root  nil "The root directory of msys2.")

  (defvar msys2-bin   nil "The executive of msys2.")

  (defvar mingw64-bin nil "The executive of mingw64.")

  (catch 'loop
    (dolist (mpath '("C:\\msys64"
                     "D:\\msys64"
                     "C:\\Applications\\msys64"
                     "D:\\Applications\\msys64"))
      (when (file-exists-p mpath)
        (setq msys2-root mpath)
        (throw 'loop t))))
  (when msys2-root
    (setq msys2-bin (concat msys2-root "\\usr\\bin"))
    (setq mingw64-bin (concat msys2-root "\\mingw64\\bin"))

    ;; Configure exec-path and PATH variables
    (setq exec-path (cons msys2-bin exec-path))
    (setq exec-path (cons mingw64-bin exec-path))
    (setenv "PATH" (concat msys2-bin ";"
                           mingw64-bin ";"
                           (getenv "PATH"))))

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

  ;; Return the button at the beginning of win other than Win+L to Emacs
  ;; see @https://emacs-china.org/t/superkey/9387
  (w32-register-hot-key [s-])

  ;;Set windows, emacs startup working directory
  (cd "~/"))

(provide 'core-os)

;;; core-os.el ends here
