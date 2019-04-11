;;; init-dired.el --- Initialize Dired               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
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

;;

;;; Code:

(require 'dired)
(setq dired-recursive-copies t) ; Recursive copying
(setq dired-recursive-deletes t) ; Recursive deletion

;; see @https://stackoverflow.com/questions/95631/open-a-file-with-su-sudo-inside-emacs
(add-hook 'dired-mode-hook
    (lambda ()
      ;; open current file as sudo
      (local-set-key (kbd "C-x <M-S-return>") (lambda()
        (interactive)
        (message "!!! SUDO opening %s" (dired-file-name-at-point))
        (lye/sudo-find-file (dired-file-name-at-point))))))

(provide 'init-dired)
;;; init-dired.el ends here
