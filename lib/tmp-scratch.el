;;; tmp-scratch.el --- Creat or switch tmp scratch buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: tmp, buffer

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

;; Creat or switch tmp scratch buffer

;;; Code:

(require 'lib-f)

(defgroup tmp-scratch nil "Tmp-scratch" :group 'tmp-scratch)

(defcustom tmp-scratch-directory (lib-f-join user-emacs-directory "tmp-scratchs")
  "Temporary *scratch* Save Folder."
  :type 'directory
  :group 'tmp-scratch)



(defun tmp-scratch--buffer-list ()
  "Get all prefixed `*scratch--' of buffer."
  (cl-remove-if-not (lambda (x) (string-prefix-p "*scratch--" x))
                    (mapcar 'buffer-name (buffer-list))))

;;;###autoload
(defun tmp-scratch-close-all-buffer ()
  "Close all the prefix buffer *scratch--."
  (interactive)
  (let ((buf-list (tmp-scratch--buffer-list)))
    (when buf-list
      (mapc (lambda (x)
              (set-buffer x)
              (when (buffer-modified-p)
                (with-temp-message
                    (with-current-buffer " *Minibuf-0*" (buffer-string))
                  (let ((inhibit-message t))
                    (basic-save-buffer))))
              (kill-buffer x))
            buf-list))))

;;;###autoload
(defun tmp-scratch-initialize-orig (&optional extname)
  "Create a temporary buffer to replace *scratch*. Use `scratch.xx' save it.
xx is EXTNAME."
  (interactive)
  (unless (file-exists-p tmp-scratch-directory)
    (make-directory tmp-scratch-directory t))
  (let* ((base-name "scratch")
         (file-ext (or extname "txt"))
         (file-name (lib-f-join tmp-scratch-directory
                                (concat base-name "." file-ext)))
         (buf-name (concat "*" base-name "--" file-ext "*")))
    (if (member buf-name (mapcar 'buffer-name (buffer-list)))
        (switch-to-buffer buf-name)
      (progn (switch-to-buffer (find-file-noselect file-name))
             (rename-buffer buf-name)))
    (setq default-directory (concat (getenv "HOME") "/"))))

;;;###autoload
(defun tmp-scratch-initialize-el ()
  "Temporary emacs-lisp test files."
  (interactive)
  (tmp-scratch-initialize-orig "el"))

;;;###autoload
(defun tmp-scratch-initialize-sh ()
  "Temporary Sh-script test files."
  (interactive)
  (tmp-scratch-initialize-orig "sh"))

;;;###autoload
(defun tmp-scratch-initialize-py ()
  "Temporary Python-script test files."
  (interactive)
  (tmp-scratch-initialize-orig "py"))

(dolist (ext '(el sh py orig))
  (let ((fun (intern (format "tmp-scratch-initialize-%s" ext)))
        (new (intern (format "lib-scratch/%s" ext))))
    (defalias new fun)))

(provide 'tmp-scratch)
;;; tmp-scratch.el ends here
