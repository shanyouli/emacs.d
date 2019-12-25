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

(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(windows-nt ms-doc)))

(defconst lye-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst lye-core-dir (concat lye-emacs-dir "core/")
  "The root directory of Lye-Emacs's core files. Must end with a slash.")

(defconst lye-library-dir (concat lye-emacs-dir "lib/")
  "The root directory of libray directory. Must end with a slash.")

;; Ensure `lye-core-dir' and `lye-library-dir'  in `load-path'
(push lye-core-dir load-path)
(push lye-library-dir load-path)

(defvar lye--initial-file-name-handler-alist file-name-handler-alist)

;; This is consulted on every `require', `load' and various path/io handling
;; encrypted or compressed files, among other things.
(setq file-name-handler-alist nil)

;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun lye--reset-file-handler-alist-h ()
  (setq file-name-handler-alist lye--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'lye--reset-file-handler-alist-h)

;; Load the bare necessities

(autoload 'lib-load-relative "lib-load")
(autoload 'lib-f-join "lib-f")
(autoload 'lib-autoload-generate-file-list "lib-autoload")

(lib-load-relative 'core/core-libs)

;; Do this on idle timer to defer a possible GC pause that could result; also
;; allows deferred packages to take advantage of these optimizations.
;; see@https://github.com/emacsmirror/gcmh
(defvar lye--gc-cons-threshold (if (display-graphic-p) #x1000000 #x400000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.
When use graphic, its value is 16Mib, otherwise 4Mib")

(defvar lye--gc-cons-upper-limit (if (display-graphic-p) #x20000000 #x8000000)
  "The temporary value for `gc-cons-threshold' to defer it.
Whe use graphic, its value is 512Mib, otherwise 128Mib.")

(defvar lye--gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(add-hook! 'emacs-startup-hook
    (setq gc-cons-threshold lye--gc-cons-threshold)
  ;; GC automatically while unfocusing the frame
  ;; `focus-out-hook' is obsolete since 27.1
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda () (unless (frame-focus-state) (garbage-collect))))
    (add-hook 'focus-out-hook #'garbage-collect))
  ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  (defun lye-minibuffer-setup-h ()
    (setq gc-cons-threshold lye--gc-cons-upper-limit))
  (defun lye-minibuffer-exit-h ()
    (setq gc-cons-threshold lye--gc-cons-threshold))
  (add-hook! 'minibuffer-setup-hook #'lye-minibuffer-setup-h)
  (add-hook! 'minibuffer-exit-hook #'lye-minibuffer-exit-h))

;;
;;; Global variables

(defconst lye-site-lisp-dir (concat lye-emacs-dir "site-lisp/")
  "The root directory of third packages. Must end with a slash.")

(defconst lye-packags-dir (concat lye-emacs-dir "packages/")
  "The root directory of package-manager, Must end with a slash.")

(defconst lye-modules-dir (concat lye-emacs-dir "modules/")
  "You don't need to load directly but use the extended key to load the package
 configuration folder.")

(defconst lye-etc-dir (concat lye-emacs-dir "etc/")
  "etc dir in `lye-emacs-dir', Must end with a slash.")

(defconst lye-emacs-cache-dir (concat lye-emacs-dir ".cache/")
  "Is the cache directory this?")

(defconst lye-emacs-share-dir (concat lye-emacs-dir "share/")
  "Store files in non-el format, such as `plantuml.jar', `pyim-bigdict.pyim.gz'.")

(defconst lye-emacs-pyim-big-file
  (concat lye-emacs-share-dir "pyim-dict/pyim-bigdict.pyim.gz")
  "Store the location of the pyim-dictionary.")

(defconst lye-emacs-plantuml-file
  (concat lye-emacs-share-dir "plantuml/plantuml.jar")
  "Store the location of the plantuml.jar.")

(defconst lye-emacs-yas-snippets-dir
  (concat lye-emacs-share-dir "snippets")
  "Store the location of the `Yas-snippets'.")

(defconst lye-emacs-custom-temp-file
  (concat lye-emacs-share-dir "custom-template.el")
  "The custom template of `custom-file'.")

;;
;;; customization
(defcustom lye-full-name "shanyouli" "Set user full name."
  :type 'string)

(defcustom lye-mail-address "shanyouli6@gmail.com"
  "Set user mail address."
  :type 'string)

(defconst lye-homepage  "https://github.com/shanyouli/emacs.d"
  "The Github page of My Emacs Configurations.")

(defcustom lye-use-fuz-or-flx-in-ivy nil
  "If it is `flx', use fuzzy match with `flx' package.
If it is `fuz', use fuzzy match with `fuz' package.
If it is `nil', Not use fuzzy match."
  :type '(choice (const :tag "fuzzy match" 'flx)
                 (const :tag "fuzzy" 'fuz)
                 (const :tag "Null" nil)))

(defcustom lye-company-enable-yas nil
  "Enable yasnippet for company backends or not."
  :type  'boolean)

(unless (file-directory-p lye-emacs-cache-dir)
  (make-directory lye-emacs-cache-dir t))

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq lib-autoload-save-directory (lib-f-join lye-emacs-cache-dir "autoloads"))
(lib-autoload-generate-file-list '((lye-core-dir . "core")
                                   (lye-site-lisp-dir . "site-lisp")
                                   (lye-modules-dir . "modules")
                                   (lye-etc-dir . "etc")
                                   (lye-library-dir . "lib")))

(lib-load-add-load-path lye-etc-dir t)
(lib-load-add-load-path lye-site-lisp-dir t)
(lib-load-add-load-path lye-modules-dir)

;;; Load `custom-file'
(setq custom-file (concat lye-emacs-cache-dir "custom.el"))

(if (and (file-exists-p lye-emacs-custom-temp-file)
         (not (file-exists-p custom-file)))
    (copy-file lye-emacs-custom-temp-file custom-file))

(if (file-exists-p custom-file) (load custom-file :no-error :no-message))

(defun lye-core-initialize ()
  "Load Lye's core files for an interactive session."
  (lib-load-relative 'core/core-benchmark) ; benchmark
  (lib-load-relative 'core/core-generic)   ; generic and delete *scratch*
  (lib-load-relative 'core/core-straight)  ; staraight, package
  (lib-load-relative 'core/core-ui)        ; UI
  (lib-load-relative 'core/core-package)   ; packages initialization
  (lib-load-relative 'core/core-modules)   ; Modeuls manager
  (lib-load-relative 'core/core-key)       ; Keybindings
  )

(provide 'core)

;;; core.el ends here
