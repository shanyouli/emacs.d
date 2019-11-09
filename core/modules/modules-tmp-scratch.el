;;; modules-tmp-scratch.el --- Substituted scratch-buffer -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tmp-scratch


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

;; `*scratch*' to replace the use of temporary files

;;; Change log:
;; 2019/11/09
;;        * Streamline `mts/scratch-initialize+' function
;;        * add `mts/create-buffer+' `mts/buffer-list' `mts/close-all-mts-buffer' function
;;        * fix the `bf-list' error
;;; Code:

(defgroup modules-tmp-scratch nil
  "Tmp-Scratch module."
  :group 'modules-tmp-scratch)

(defcustom mts-directory
  (expand-file-name "tmp-scratch/"
                    (if (boundp 'lye-emacs-cache-dir)
                        lye-emacs-cache-dir
                      user-emacs-directory))
  "Temporary *scratch* Save Folder."
  :type 'directory
  :group 'modules-tmp-scratch)



(defun mds/create-buffer+ (file-name)
  "Known file name to create a buffer name."
  (concat "*"
          (mapconcat 'identity (split-string file-name "\\.") "-")
          "*"))

(defun mts/buffer-list ()
  "Get all prefixed `*mts' of buffer."
  (remove-if-not (lambda (x)
                   (string-prefix-p "*mts-" x))
                 (mapcar 'buffer-name (buffer-list))))

;;;###autoload
(defun mts/scratch-initialize+ (&optional file)
  "Create a temporary buffer to replace *scratch*. Use `file' save it"
  (interactive)
  (unless (file-directory-p mts-directory)
    (make-directory mts-directory t))

  (let* ((file (expand-file-name (or file "mts.txt") mts-directory))
         (buf-list (mts/buffer-list))
         (buf-name (concat "*"
                           (mapconcat 'identity
                                      (split-string (file-name-nondirectory file) "\\.") "-")
                           "*")))
    (if (and buf-list (member buf-name buf-list))
        (switch-to-buffer buf-name)
      (switch-to-buffer (find-file file))
      (rename-buffer buf-name))
    (setq default-directory (concat (getenv "HOME") "/"))))

;;;###autoload
(defun mts/scratch-initialize-el ()
  "Temporary emacs-lisp test files."
  (interactive)
  (mts/scratch-initialize+ "mts.el"))

;;;###autoload
(defun mts/scratch-initialize-sh ()
  "Temporary sh-script test files."
  (interactive)
  (mts/scratch-initialize+ "mts.sh"))

;;;###autoload
(defun mts/scratch-initialize-py ()
  "Temporary Python-script test files."
  (interactive)
  (mts/scratch-initialize+ "mts.py"))

(defun mts/close-all-mts-buffer ()
  "Close all the prefix buffer *mts-"
  (interactive)
  (let ((mts-buf-list (mts/buffer-list)))
    (mapc (lambda (x)
            (set-buffer x)
            (when (and (buffer-file-name)
                       (buffer-modified-p))
              (with-temp-message
                  (with-current-buffer " *Minibuf-0*" (buffer-string))
                (let ((inhibit-messsage t))
                  (basic-save-buffer))))
            (kill-buffer x))
          mts-buf-list)))

(provide 'modules-tmp-scratch)

;;; modules-tmp-scratch.el ends here
