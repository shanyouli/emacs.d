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
(defvar lye-require-initialize nil
  "Avoid loading init-const and init-custom multiple times.")

(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-percentage 0.1)
            (setq gc-cons-threshold (* 2 1000 1000))

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
              (setq gc-cons-threshold (* 2 1024 1024)))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force `lisp' at the head to reduce the startup time.
(load-file "~/.emacs.d/lisp/init-load-path.el")

(require 'use-package)

(require 'init-const)
(require 'init-custom)

;; Test and optimize startup
(when lye-enable-benchmark
  (use-package benchmark-init
    :commands bechmark-init/deactivate
    :ensure nil
    :hook (after-init . bechmark-init/deactivate)))

;; Import self-configuration of different systems
(let ((system-file (format "%s/init-%s.el"
                           (expand-file-name "lisp" user-emacs-directory)
                           (symbol-name system-type))))
  (if (file-exists-p system-file)
      (load-file system-file)))

(require 'init-font)                    ; font set
(require 'init-ui)                      ; frame size set
(require 'init-modeline)                ; modeline
(require 'init-theme)                   ; load theme
(require 'init-scratch)                 ; scratch configure
(require 'init-funcs)                   ; some useful functions
(require 'init-basic)

(with-temp-message ""                   ; Erase the output of the plugin startup

  (require 'init-package)               ; Package

  ;; Preferences
  (require 'init-edit)
  (require 'init-ivy)
  (require 'init-window)
  (require 'init-company)

  ;; Tools
  (require 'init-magit)                 ; Git
  (require 'init-dired)                 ; Dired
  (require 'init-elfeed)                ; RSS Reader
  ;; (require 'init-eshell)

  (require 'init-chinese)

  ;; Program language common tool
  (require 'init-lang)
  (require 'init-flycheck)
  (require 'init-elisp)
  (require 'init-scheme)
  (require 'init-lua)
  (require 'init-lsp)
  (require 'init-python)

  ;; Org mode
  (require 'init-hugo)
  (require 'init-org))

;; get emascs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(provide 'init)
;;; init.el ends here
