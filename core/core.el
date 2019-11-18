;;; core.el --- Initialize Load Path -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Homepage: https://github.com/shanyouli/emacs.d


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

;;; Code:

(when (version< emacs-version "26.1")
  (error "Detected Emacs %s. Lye-Emacs only supportss Emacs 16.2 and higher"
         emacs-version))

(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-doc)))

;; Ensure `lye-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(defvar lye--initial-file-name-handler-alist file-name-handler-alist)

;;
;;; Global variables

(defvar lye--gc-cons-threshold 33554432 ; 32Mib
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

;;; Directories/files
(defconst lye-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")


(defconst lye-core-dir (concat lye-emacs-dir "core/")
  "The root directory of Lye-Emacs's core files. Must end with a slash.")

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq file-name-handler-alist nil)

(lye/core-require 'modules-autoload t)
(setq md-autoload-load-dir-alist
      '((lye-emacs-core-dir . "core")
        (lye-emacs-site-lisp-dir . "site-lisp")
        (lye-emacs-modules-dir . "modules")))

(md/autoload-create-and-load-file-list)

(setq md-autoload-load-path-list '(lye-emacs-core-dir
                                   lye-emacs-core-modules-dir
                                   lye-emacs-modules-dir
                                   (lye-emacs-site-lisp-dir)))
(md/autoload-add-load-path-list)

;; init Dired
(defmacro lye/init-require (pkg)
  "Import the `*.el' file in the lye-emacs-lisp-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-init-dir ,pkg)))

;;; Add after-load-theme-hook
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :after #'run-after-load-theme-hook)

;;; Add lye-init-hook
(defvar lye-init-hook nil
  "HOOK runs after after-start-hook.")

(defun run-lye-init-hook (&rest _)
  "Run `lye-init-hook'."
  (run-at-time 0.1 nil (lambda () (run-hooks 'lye-init-hook))))

(add-hook 'after-init-hook #'run-lye-init-hook)

(provide 'core)

;;; core.el ends here
