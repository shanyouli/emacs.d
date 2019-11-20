;;; init.el --- Initialize startup  -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.2
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: init


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

;; Initialize startup

;;; Code:

(eval-when-compile
  (if (version< emacs-version "25.3")
      (error "Detected Emacs %s. Lye-emacs only supports Emacs 25.3 and higher."
             emacs-version)))

(defvar lye--gc-cons-threshold (* 20 1024 1024)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar lye--file-name-handler-alist file-name-handler-alist)

(defun lye/restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist lye--file-name-handler-alist)
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
   3 nil
   (lambda ()
     (setq-default gc-cons-threshold lye--gc-cons-threshold)
     ;; To speed up minibuffer commands (like helm and ivy), we defer garbage
     ;; collection while the minibuffer is active.
     (defun lye/defer-garbage-collection ()
       (setq gc-cons-threshold most-positive-fixnum))
     (defun lye/restore-garbage-collection ()
       ;; Defer it so that commands launched from the minibuffer can enjoy the
       ;; benefits.
       (run-at-time 1 nil (lambda () (setq gc-cons-threshold lye--gc-cons-threshold))))
     (add-hook 'minibuffer-setup-hook #'lye/defer-garbage-collection)
     (add-hook 'minibuffer-exit-hook #'lye/restore-garbage-collection)
     ;; GC all sneaky breaky like
     (add-hook 'focus-out-hook #'garbage-collect))))

(if (ignore-errors (or after-init-time noninteractive))
    (setq gc-cons-threshold lye--gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `lye/restore-startup-optimizations'
  (setq gc-cons-threshold most-positive-fixnum)
  ;; This is consulted on every `require', `load' andvarious path/io functions.
  ;; you get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
  (add-hook 'emacs-startup-hook #'lye/restore-startup-optimizations))

;; In noninteractive sessions,prioritize non-byte-compiled source files to
;; prevent stable, byte-compiled code from running. However, if you're getting
;; recursive load errors, it may help to set this to nil.
(setq load-prefer-newer noninteractive)

;; Let 'er rip!

;; Enusre Lye-Emacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(require 'core-custom ; Variables
         (expand-file-name "core/core-custom.el" user-emacs-directory))

(lye/core-require 'core-benchmark)      ; benchmark

(lye/core-require 'core)                ; `load-path', Variables, benchmark
(lye/core-require 'core-generic)        ; generic and delete *scratch*
(lye/core-require 'core-straight)       ; staraight, package
(lye/core-require 'core-key)            ; Keybindings
(lye/core-require 'core-ui)             ; UI
(lye/core-require 'core-package)        ; packages initialization

(lye/init-require 'init-key)          ; Keybindings
(lye/init-require 'init-edit)         ; better edit
(lye/init-require 'init-shackle)      ; Window rule
(lye/init-require 'init-dired)        ; Dired
(lye/init-require 'init-reads)        ; Reader tools
(lye/init-require 'init-company)      ; company

(run-with-idle-timer 0.1 nil
                     (lambda ()
                       ;; Program language common tool
                       (lye/init-require 'init-lang)
                       (lye/init-require 'init-elisp)
                       (lye/init-require 'init-scheme)
                       (lye/init-require 'init-sh)
                       ;;(lye/init-require 'init-lua)
                       (lye/init-require 'init-python)))
;; Org mode
(run-with-idle-timer 1 nil
                     (lambda ()
                       (lye/init-require 'init-hugo)
                       (lye/init-require 'init-org)))
