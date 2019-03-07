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
(let (
      ;; Temporarily increase `gc-cons-threhold' when loading
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))
  ;;(defvar default-file-name-handler-alist file-name-handler-alist)
  ;;(setq file-name-handler-alist nil)
  ;;(setq gc-cons-threshold 80000000)
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda ()
  ;;             "Restore default values after init."
  ;;             (setq file-name-handler-alist default-file-name-handler-alist)
  ;;             (setq gc-cons-threshold 800000)
  ;;             (add-hook 'focus-out-hook 'garbage-collect)))
  ;; Load path
  ;; Optimize: Force `lisp' at the head to reduce the startup time.
  (defun update-load-path (&rest _)
    "Update `load-path'."
    (push (expand-file-name "lisp" user-emacs-directory) load-path))
  (advice-add #'package-initialize :after #'update-load-path)
  (if (version< emacs-version "27.0")
      (update-load-path)
    (push (expand-file-name "lisp" user-emacs-directory) load-path))
  
  ;; Constants
  (defconst lye-homepage  "https://github.com/lye95/emacs.d"
    "The Github page of My Emacs Configurations.")
  (defconst *is-a-win* (eq system-type 'windows-nt)
    "Are we running on a Windows System?")

  (defconst *is-a-mac* (eq system-type 'darwin)
    "Are we running on a Mac System?")

  (defconst *is-a-linux* (eq system-type 'gnu/linux)
    "Are we running on a GNU/Linux System?")

  (defconst *root* (string-equal "root" (getenv "USER"))
    "Are you using ROOT user?")

  ;; Set the temporal directory
  (defconst lye-emacs-temporal-dir (concat user-emacs-directory "tmp/")
    "Is the temporal diirectory this?")

  ;; customization
  (defcustom lye-full-name "lye li"
    "Set user full name."
    :type 'string)

  (defcustom lye-mail-address "shanyouli6@gmail.com"
    "Set user mail address."
    :type 'string)

  ;;Set package archives from which to fetch
  (defcustom lye-package-archives 'emacs-china
    "Set package archives from which to fetch."
    :type '(choice (const :tag "Melpa" melpa)
	               (const :tag "Melpa-mirror" melpa-mirror)
	               (const :tag "Emacs-china" emacs-china)
	               (const :tag "Netease" netease)))

  (defcustom lye-company-enable-yas nil
    "Enable yasnippet for company backends or not."
    :type  'boolean)

  ;; Set theme
  (defcustom lye-themes 'default
    "Set color theme."
    :type '(choice (const :tag "Monokai Theme" default)
	               (const :tag "Tao theme" light)
	               (const :tag "Tao theme" dark)))
  ;; Load `custom-file'
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) (load custom-file))
  
  (with-temp-message ""              ; Erase the output of the plugin startup

    ;; Set the color at startup to avoid flickering
    (if (display-graphic-p)
        (custom-set-faces
         '(default ((t (:background "#282c34" :foreground "#bbc2cf")))))
      (custom-set-faces
       '(default ((t (:background "black" :foreground "#137D11"))))))

    (require 'init-package)          ; Packages

    ;; Preferences
    (require 'init-ui)
    (require 'init-basic)
    (require 'init-edit)
    (require 'init-funcs)
    (require 'init-window)
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
    (require 'init-markdown)))

(provide 'init)
;;; init.el ends here
