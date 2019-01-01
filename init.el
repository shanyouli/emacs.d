;;; init.el --- Lye Emacs configurations.            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye

;; Author: lye <shanyouli6@gemail.com>
;; Keywords: .emacs.d lye

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

;; Lye Emacs Configurations

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)
            (add-hook 'focus-out-hook 'garbage-collect)))

;; Load path
;; Optimize: Force `lisp' at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

;; Constants
(require 'init-const)

;; Customization
(require 'init-custom)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-basic)
(require 'init-edit)
(require 'init-funcs)
(require 'init-ui)
(require 'init-window)
(require 'init-modeline)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-chinese)
(require 'init-flycheck)
(require 'init-eshell)
(require 'init-magit)
;;(require 'init-pair)

;; RSS Reader
(require 'init-elfeed)

(when *is-a-win*
  (require 'init-ahk))

(require 'init-elisp)
(require 'init-org)
(require 'init-scheme)
(require 'init-lsp)
(require 'init-sh)


(provide 'init)
;;; init.el ends here
