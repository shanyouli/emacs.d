;;; core-site-lisp.el --- Add site-lisp files in the directory to load-path -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Homepage: https://github.com/shany/emacs.d


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

;;; Code:
(require 'core-funcs)

(defvar lye-emacs-site-lisp-autoload
      (expand-file-name (concat (+get-dir-name-nondirectory lye-emacs-site-lisp-dir)
                                "-loadfs.el")
                    lye-emacs-autoload-dir))

(defun +add-site-lisp-to-load-path ()
  "Add site-lisp path to the `load-path'"
  (mapc #'add-to-load-path
        (+find-subdir-recursively lye-emacs-site-lisp-dir)))

(defun site-lisp-autoload-file-refresh ()
  (interactive)
  (generate-autoload-and-refresh lye-emacs-site-lisp-dir lye-emacs-site-lisp-autoload))

(defun +site-lisp-initialized ()

  (+add-site-lisp-to-load-path)
  (unless (file-exists-p lye-emacs-site-lisp-autoload)
    (site-lisp-autoload-file-refresh))
  (load lye-emacs-site-lisp-autoload :no-error :no-message))

(+site-lisp-initialized)

(provide 'core-site-lisp)

;;; core-site-lisp.el ends here
