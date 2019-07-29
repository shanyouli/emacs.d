;;; lex-temp.el --- Replace the temporary file of scratch-buffer -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version:v0.1
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

;; Replace the temporary file of *Scratch-buffer*

;;; Code:

(defvar temp-scratch-dir (expand-file-name "scratch/" lye-emacs-cache-dir)
  "Store different scratch temporary files.")

(defvar temp-scratch-file (expand-file-name "scratch" temp-scratch-dir)
  "The temp file.")

(defvar temp-scratch-sh-file (expand-file-name "temp.sh" temp-scratch-dir)
  "The temp about shell script.")

(defvar temp-scratch-el-file (expand-file-name "temp.el" temp-scratch-dir)
  "The temp file about emacs-lisp.")

(defun temp-scratch-init (&optional file)
  "Create a temporary buffer to replace *scratch*."
  (interactive)

  (unless (file-directory-p temp-scratch-dir)
    (make-directory temp-scratch-dir))

  (if file
      (progn
        (switch-to-buffer (find-file file))
        (cond
         ((string-match "\\.sh" file)
          (if (member "*temp-sh*" (mapcar 'buffer-name (buffer-list)))
              (switch-to-buffer "*temp-sh*")
            (rename-buffer "*temp-sh*")))
         ((string-match "\\.el" file)
          (if (member "*temp-el*" (mapcar 'buffer-name (buffer-list)))
              (switch-to-buffer "*temp-el*")
            (rename-buffer "*temp-el*")))
         ))

    (switch-to-buffer (find-file temp-scratch-file))
    (rename-buffer "*temp*"))

  (setq default-directory (concat (getenv "HOME") "/")))

(defun temp-scratch-init-sh ()
  (interactive)
  (temp-scratch-init temp-scratch-sh-file))

(defun temp-scratch-init-el ()
  (interactive)
  (temp-scratch-init temp-scratch-el-file))

;; key
(when (locate-library "one-key")

  (defvar one-key-menu-scratch-alist nil
    "The one-key menu of `temp-file-scratch'")

  (setq one-key-menu-scratch-alist
        '((("SPC" . "Original TBuffer") . temp-scratch-init)
          (("e" . "ELisp TBuffer") . temp-scratch-init-el)
          (("s" . "Shell TBuffer") . temp-scratch-init-sh)))
  (defun one-key-menu-scratch-buffer ()
    "The one-key Menu of `Temp-file-scratch'"
    (interactive)
    (one-key-menu "Temp Buffer" one-key-menu-scratch-alist))
  )

(provide 'lex-temp)

;;; lex-temp.el ends here
