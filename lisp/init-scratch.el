;;; init-scratch.el ---Scratch Configurations        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords: scratch

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

;; scratch

;;; Code:

;; Set scratch default major-mode
(setq initial-major-mode 'emacs-lisp-mode)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, "
                      user-login-name " - Emacs ♥ you!\n\n"))

;; scratch-save
;; see @https://emacs-china.org/t/topic/4714/2?u=twlz0ne
(defvar lye-scratch-save-file (concat lye-emacs-cache-dir "scratch"))

(defun lye/scratch-save ()
  (ignore-errors
    (with-temp-message (with-current-buffer " *Minibuf-0*" (buffer-string))
      (with-current-buffer "*scratch*"
        (write-region nil nil lye-scratch-save-file)))))

(defun lye/scratch-restore()
  (let ((f lye-scratch-save-file))
    (with-current-buffer "*scratch*"
      (erase-buffer)
      (if (file-exists-p f)
          (insert-file-contents f)
        (insert initial-scratch-message))
      (goto-char (point-max)))))

(add-hook 'kill-emacs-hook #'lye/scratch-save)
(add-hook 'after-init-hook #'lye/scratch-restore)

;; Make *Scratch* buffer undelete
(defun lye/unkillable-scratch-buffer ()
  "Don't delete *scratch*."
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        ;; (insert initial-scratch-message)
        (lye/scratch-restore)
        nil)
    t))
(add-hook 'kill-buffer-query-functions #'lye/unkillable-scratch-buffer)

;; default *Scratch*
(defun lye/reset-scratch-buffer ()
  "Reset *Scratch* buffer!"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(provide 'init-scratch)
;;; init-scratch.el ends here
