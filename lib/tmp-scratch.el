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

(defcustom tmp-scratch-lang-alist '(("zsh" . "sh")
                                    ("bash" "sh")
                                    ("shell" "sh")
                                    ("python" . "py")
                                    ("emacs-lisp" . "el")
                                    ("elisp" . "el")
                                    ("txt" . "txt")
                                    ("orign" . "txt"))
  "常用的语言和对应文件名后缀."
  :type 'alist
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

(defun tmp-scratch--initialize (extname)
  "Create a temporary buffer to replace *scratch*. Use `scratch.xx' save it.
xx is EXTNAME."
  (unless (file-exists-p tmp-scratch-directory)
    (make-directory tmp-scratch-directory t))
  (let* ((base-name "scratch")
         (file-ext extname)
         (file-name (lib-f-join tmp-scratch-directory
                                (concat base-name "." file-ext)))
         (buf-name (concat "*" base-name "--" file-ext "*")))
    (if (member buf-name (mapcar 'buffer-name (buffer-list)))
        (switch-to-buffer buf-name)
      (progn (switch-to-buffer (find-file-noselect file-name))
             (rename-buffer buf-name)))
    (setq default-directory (concat (getenv "HOME") "/"))))

;;;###autoload
(defmacro tmp-scratch-create-fun! (prog-lang &optional ext-name)
  (declare (indent 1))
  (let* ((prog-lang (tmp-scratch--lang-symbol prog-lang))
         (func-name (intern (format "tmp-scratch-initialize/%s" prog-lang)))
         (alias-func (intern (format "lib-scratch/%s" prog-lang)))
         (ext-name (or ext-name (alist-get prog-lang tmp-scratch-lang-alist
                                           nil nil 'equal))))
    `(progn
       (defun ,func-name ()
         (interactive)
         (tmp-scratch--initialize ,ext-name))
       (defalias ',alias-func ',func-name))))

(defun tmp-scratch--lang-symbol (prog-lang)
  (pcase prog-lang
    ((pred symbolp) (symbol-name prog-lang))
    ((pred stringp) prog-lang)
    ((pred listp) (symbol-name (cadr prog-lang)))
    (_ (error "Cannot make into prog-lang symbol: %s" prog-lang))))

(provide 'tmp-scratch)
;;; tmp-scratch.el ends here
