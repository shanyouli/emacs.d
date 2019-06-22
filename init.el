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

;;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-percentage 0.1)
            (setq gc-cons-threshold 800000)

            ;;GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))


            ;; Avoid GCs while using `ivy' `counsel'/ `swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold most-positive-fixnum)
              (setq gc-cons-percentage 0.6))

            (defun my-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-percentage 0.1)
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

  ;; Load path
  ;; Optimize: Force `lisp' at the head to reduce the startup time.
  (defun update-load-path (&rest _)
    "Update `load-path'."
    (push (expand-file-name "lisp" user-emacs-directory) load-path))

  ;; Add the package in the extensions folder to `load-path'
  (defun add-extensions-to-load-path (&rest _)
    (eval-when-compile (require 'cl))
    (if (fboundp 'normal-top-level-add-to-load-path)
        (let* ((my-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))
               (default-directory my-lisp-dir))
          (progn
            (setq load-path
                  (append
                   (loop for dir in (directory-files my-lisp-dir)
                         unless (string-match "^\\." dir)
                         collecting (expand-file-name dir))
                   load-path))))))

  (advice-add #'package-initialize :after #'update-load-path)
  (advice-add #'package-initialize :after #'add-extensions-to-load-path)

  (update-load-path)
(add-extensions-to-load-path)

;; Constants
(require 'init-const)

;; Customization
(require 'init-custom)

;; Test and optimize startup
(when lye-enable-benchmark
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate))

(require 'init-font)                    ; font set
(require 'init-ui)                      ; frame size set
(require 'init-scratch)                 ; scratch configure
(require 'init-funcs)                   ; some useful functions

(with-temp-message ""                   ; Erase the output of the plugin startup

  (require 'init-package)               ; Package and functions


  ;; Preferences
  (require 'init-theme)
  (require 'init-elisp)
  (require 'init-basic)
  (require 'init-edit)

  (require 'init-window)
  (require 'init-company)
  ;; (require 'init-eshell)
  (require 'init-magit)
  (require 'init-dired)
  (require 'init-chinese)

  (require 'init-flycheck)
  (require 'init-yasnippet)

  (unless system/windows (require 'init-ivy))
  (require 'init-pyim)

  (if system/windows (require 'init-ahk)) ; windows-system

  (require 'init-elfeed) ; RSS Reader
  (require 'init-lang)
  (require 'init-hugo)
  (require 'init-org)
  (require 'init-scheme)
  (require 'init-python)
  (require 'init-lua)
  (require 'init-lsp))

(provide 'init)
;;; init.el ends here
