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
(setq gc-cons-threshold (* 192 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 32 1024 1024))
            (add-hook 'focus-out-hook 'garbage-collect)))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(add-to-list 'load-path (expanda-file-name "site-lisp" user-emacs-directory))

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
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-chinese)
(require 'init-flycheck)
(require 'init-eshell)
(require 'init-magit)
;;(require 'init-pair)

(require 'init-elisp)
(require 'init-org)
(require 'init-scheme)
;;(require 'init-lsp)

(provide 'init)
;;; init.el ends here
