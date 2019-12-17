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
(defconst IS-WINDOWS (memq system-type '(windows-nt ms-doc)))

(defconst lye-core-dir
  (file-truename (if load-file-name
                     (file-name-directory load-file-name)
                   (concat user-emacs-directory "core/")))
  "The root directory of Lye-Emacs's core files. Must end with a slash.")

;; Ensure `lye-core-dir' is in `load-path'
(add-to-list 'load-path lye-core-dir)

;; This is consulted on every `require', `load' and various path/io handling
;; encrypted or compressed files, among other things.
(defvar lye--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun lye--reset-file-handler-alist-h ()
  (setq file-name-handler-alist lye--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'lye--reset-file-handler-alist-h)

;; Load the bare necessities
(require 'core-libs)

;; Do this on idle timer to defer a possible GC pause that could result; also
;; allows deferred packages to take advantage of these optimizations.
(defvar lye--gc-cons-threshold  20971520  ; 20M (* 20 1024 1024)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defun lye/restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise)"
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer! :defer 3
    (setq-default gc-cons-threshold lye--gc-cons-threshold
                  gc-cons-percentage 0.1)
    ;; To speed up minibuffer commands (like helm and ivy), we defer garbage
    ;; collection while the minibuffer is active.
    (defun lye/defer-garbage-collection ()
      (setq gc-cons-threshold most-positive-fixnum))
    (defun lye/restore-garbage-collection ()
      ;; Defer it so that commands launched from the minibuffer can enjoy the
      ;; benefits.
      (run-at-time 1 nil (lambda () (setq gc-cons-threshold lye--gc-cons-threshold))))
    (add-hook! 'minibuffer-setup-hook #'lye/defer-garbage-collection)
    (add-hook! 'minibuffer-exit-hook #'lye/restore-garbage-collection)
    ;; GC all sneaky breaky like
    (add-hook! 'focus-out-hook #'garbage-collect)))

;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook! 'emacs-startup-hook #'lye/restore-startup-optimizations)

;;
;;; Global variables
(defconst lye-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst lye-emacs-site-lisp-dir (concat lye-emacs-dir"site-lisp/")
  "The root directory of third packages. Must end with a slash.")

(defconst lye-core-modules-dir (concat lye-core-dir "modules/")
  "modules dir in `lye-core-dir', Must end with a slash.")

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

(defcustom lye-emacs-autoload-dir (concat lye-emacs-cache-dir "autoload/")
  "Automatic generation autoload file storage directory."
  :type 'directory)

(dolist (d (list lye-emacs-cache-dir
                 lye-emacs-autoload-dir))
  (unless (file-directory-p d)
    (make-directory d t)))

;;; Load `custom-file'
(setq custom-file (concat lye-emacs-cache-dir "custom.el"))

(if (and (file-exists-p lye-emacs-custom-temp-file)
         (not (file-exists-p custom-file)))
    (copy-file lye-emacs-custom-temp-file custom-file))

(if (file-exists-p custom-file) (load custom-file :no-error :no-message))

;; https://github.com/honmaple/dotfiles/blob/571d6f0dca10015886c56a1feab17f0d5a1bb1ab/emacs.d/init.el#L51
(defmacro lye/core-require (pkg &optional modulep)
  "Load PKG. When MODULEP is non-nil, the presence of PKG using directory
`lye-core-modules-dir', and vice versa for `lye-core-dir'."
  (let ((dir (if modulep
                 lye-core-modules-dir
               lye-core-dir)))
    `(require ,pkg (concat ,dir (format "%s" ,pkg)))))

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
;; (setq file-name-handler-alist nil)

(lye/core-require 'modules-autoload t)

(setq md-autoload-load-dir-alist '((lye-core-dir . "core")
                                   (lye-emacs-site-lisp-dir . "site-lisp")
                                   (lye-modules-dir . "modules")
                                   (lye-etc-dir . "etc")))

(md/autoload-create-and-load-file-list)

(setq md-autoload-load-path-list '(lye-core-modules-dir
                                   lye-modules-dir
                                   (lye-etc-dir)
                                   (lye-emacs-site-lisp-dir)))
(md/autoload-add-load-path-list)

(push (concat lye-emacs-dir "lib") load-path)

(provide 'core)

;;; core.el ends here
