;;; core-autoload.el --- autoload -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (cl subr-x)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: autoload


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

;; Generate autoload files for third-party packages

;;; Code:

;; Automatically generate autoload contained in third-party packages
;; https://emacs-china.org/t/autoloads/8775/10?u=shanyouli

(defun cm/find-subdir-recursively (dir)
  "Find all subdirectories in DIR.
Dot-directories and directories contain `.nosearch' will be skipped."
  (eval-when-compile (require 'subr-x)
                     (require 'cl-seq))

  (thread-last (directory-files dir nil)
    (cl-remove-if (lambda (f)
                    (string-prefix-p "." f)))
    (mapcar (lambda (d) (expand-file-name d dir)))
    (cl-remove-if-not #'file-directory-p)
    (cl-remove-if (lambda (d)
                    (file-exists-p (expand-file-name ".nosearch"
                                                     d))))))

(defun cm/find-el-file-recursively (dir)
  "Find all `.el' files in DIR and its subdirectories."
  (let ((elfiles (directory-files dir t "\\.el\\'"))
        (subdir (cm/find-subdir-recursively dir)))
    (nconc elfiles
           (mapcan #'cm/find-el-file-recursively subdir))))

(defun generate-autoload-and-refresh (&optional target dir)
  (interactive)
  (require 'autoload)
  (let* ((target (or target lye-emacs-autoload-file))
         (dir (or dir lye-emacs-site-lisp-dir))
         (generated-autoload-file target))
    (with-temp-file target
      (dolist (f (cm/find-el-file-recursively dir))
        (let ((generated-autoload-load-name (file-name-sans-extension f)))
          (autoload-generate-file-autoloads f (current-buffer))))
      (insert (string-join `(,(char-to-string ?\C-l)
                             ";; Local Varibles:"
                             ";; version-control: never"
                             ";; no-byte-compile: t"
                             ";; no-update-autoloads: t"
                             ";; coding: utf-8"
                             ";; End:"
                             ,(format ";;; %s ends here"
                                      (file-name-nondirectory target)))
                           "\n")))))

(defun lye/load-autoload-file  ()
  "If lye-emacs-autoload-file does not exist, create it. Then load it,
otherwise just load it."
  (unless (file-exists-p lye-emacs-autoload-file)
    (generate-autoload-and-refresh))
  (load lye-emacs-autoload-file :no-error :no-message))

(lye/load-autoload-file)

(provide 'core-autoload)

;;; core-autoload.el ends here
