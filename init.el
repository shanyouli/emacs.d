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
(let ( ;; Temporarily increase `gc-cons-threhold' when loading
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))

  (defun lye/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun lye/minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'lye/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'lye/minibuffer-exit-hook)

  ;; Load path
  ;; Optimize: Force `lisp' at the head to reduce the startup time.
  (defun update-load-path (&rest _)
    "Update `load-path'."
    (push (expand-file-name "lisp" user-emacs-directory) load-path))

  ;; Add the package in the extensions folder to `load-path'
  (defun add-extensions-to-load-path (&rest _)
    (eval-when-compile (require 'cl))
    (if (fboundp 'normal-top-level-add-to-load-path)
        (let* ((my-lisp-dir (expand-file-name "extensions/" user-emacs-directory))
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


  (require 'init-const)
  (require 'init-custom)
  (require 'init-ui)
  (require 'init-scratch)
  (require 'init-funcs)

  (when lye-enable-benchmark
    ;; Test and optimize startup
    (require 'benchmark-init-modes)
    (require 'benchmark-init)
    (benchmark-init/activate))

  (with-temp-message "" ; Erase the output of the plugin startup

    ;; Package and functions
    (require 'init-package)

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

    (unless system/windows
      (require 'init-ivy)
      (require 'init-pyim))

    (if system/windows (require 'init-ahk)) ; windows-system

    (run-with-idle-timer
     1 nil
     #'(lambda ()
         (require 'init-elfeed) ; RSS Reader
         (require 'init-lang)
         (require 'init-hugo)
         (require 'init-org)
         (require 'init-scheme)
         (require 'init-python)
         (require 'init-lua)
         (require 'init-lsp)
         ))))

(provide 'init)
;;; init.el ends here
