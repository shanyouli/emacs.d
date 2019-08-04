;;; init.el --- Initialize startup  -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
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

(defvar lye-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar lye-gc-cons-upper-linit most-positive-fixnum        ;;536870921 ; 512mb
  "The temporary value for `gc-cons-threshold' to defer it.")

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
     (setq-default gc-cons-threshold lye-gc-cons-threshold)
     ;; To speed up minibuffer commands (like helm and ivy), we defer garbage
     ;; collection while the minibuffer is active.
     (defun lye/defer-garbage-collection ()
       (setq gc-cons-threshold lye-gc-cons-upper-linit))
     (defun lye/restore-garbage-collection ()
       ;; Defer it so that commands launched from the minibuffer can enjoy the
       ;; benefits.
       (run-at-time 1 nil (lambda () (setq gc-cons-threshold lye-gc-cons-threshold))))
     (add-hook 'minibuffer-setup-hook #'lye/defer-garbage-collection)
     (add-hook 'minibuffer-exit-hook #'lye/restore-garbage-collection)
     ;; GC all sneaky breaky like
     (add-hook 'focus-out-hook #'garbage-collect))))

(if (ignore-errors (or after-init-time noninteractive))
    (setq gc-cons-threshold lye-gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `lye/restore-startup-optimizations'
  (setq gc-cons-threshold lye-gc-cons-upper-linit)
  ;; This is consulted on every `require', `load' andvarious path/io functions.
  ;; you get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
  (add-hook 'after-init-hook #'lye/restore-startup-optimizations))

;; In noninteractive sessions,prioritize non-byte-compiled source files to
;; prevent stable, byte-compiled code from running. However, if you're getting
;; recursive load errors, it may help to set this to nil.
(setq load-prefer-newer noninteractive)

;; Let 'er rip!
(eval-when-compile
  (if (version< emacs-version "25.3")
      (error "Detected Emacs %s. Lye-emacs only supports Emacs 25.3 and higher."
             emacs-version)))

;; https://github.com/honmaple/dotfiles/blob/571d6f0dca10015886c56a1feab17f0d5a1bb1ab/emacs.d/init.el#L51
(defmacro lye/core-require (pkg)
  "Load PKG."
  `(require ,pkg (format "%s/%s.el" (concat user-emacs-directory  "core") ,pkg)))

(lye/core-require 'core)                ; `load-path', Variables, benchmark
(lye/core-require 'core-key)            ; Keybindings
(lye/core-require 'core-generic)        ; generic
(lye/core-require 'core-scratch)        ; scratch
(lye/core-require 'core-ui)             ; UI
(lye/core-require 'core-modeline)       ; mode-line
(lye/core-require 'core-os)             ; OS environmental variable
(lye/core-require 'core-package)        ; packages initialization
(lye/core-require 'core-awesome-tab)    ; awesome-tab
(lye/core-require 'core-elpa)           ; package management tool

(with-temp-message ""                   ; Erase the output of the plugin startup
  (lye/init-require 'init-hydras)
  (lye/init-require 'init-key)          ; Keybindings

  ;; Preferences
  (lye/init-require 'init-edit)
  (lye/init-require 'init-shackle)      ; Window rule

  ;; Tools
  (lye/init-require 'init-dired)        ; Dired
  (lye/init-require 'init-reads)        ; Reader tools
  (lye/init-require 'init-company)

  (run-with-idle-timer 0.1 nil  (lambda ()
                                  ;; Program language common tool
                                  (lye/init-require 'init-lang)
                                  (lye/init-require 'init-elisp)
                                  (lye/init-require 'init-scheme)
                                  (lye/init-require 'init-sh)
                                  (lye/init-require 'init-lua)
                                  (lye/init-require 'init-python)))
  ;; Org mode
  (run-with-idle-timer 1 nil (lambda ()
                               (lye/init-require 'init-hugo)
                               (lye/init-require 'init-org))))

;; get emascs startup time
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; load all el-file in lye-emacs-modules-dir, When fist run.
(unless lye-load-all-module-file-p
  (run-with-idle-timer 2 nil (lambda ()
  (lye/core-require 'core-funcs)
  (load-all-module-file)
  (if (file-exists-p custom-file)
      (append-to-file "\n (setq lye-load-all-module-file-p t)" nil custom-file)))))


(provide 'init)

;;; init.el ends here
