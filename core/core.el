;;; core.el --- Initialize Load Path -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (cl)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: load-path


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

;; Initialize Load-Path

;;; Code:

;; depends
(require 'cl-macs)
(require 'cl-seq)
;; constant

(defconst system/windows (memq system-type '(windows-nt ms-dos cygwin))
  "Are we running on a Windows System?")

(defconst system/mac (eq system-type 'darwin)
  "Are we running on a Mac System?")

(defconst system/linux (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux System?")

(defconst *root* (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst lye-emacs-site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)
  "The root directory of third packages.")

(defconst lye-emacs-core-dir (expand-file-name "core/" user-emacs-directory)
  "Initialize some packages that are not installed using package.el.")

(defconst lye-emacs-init-dir (expand-file-name "lisp/" user-emacs-directory)
  "Initialize some packages that are installed using package.el.")

(defconst lye-emacs-modules-dir (expand-file-name "modules/" user-emacs-directory)
  "You don't need to load directly but use the extended key to load the package
 configuration folder.")

(defconst lye-emacs-cache-dir (concat user-emacs-directory ".cache/")
  "Is the cache directory this?")

(defconst lye-emacs-share-dir (expand-file-name "share/" user-emacs-directory)
  "Store files in non-el format, such as `plantuml.jar', `pyim-bigdict.pyim.gz'.")

(defconst lye-emacs-pyim-big-file
  (expand-file-name "pyim-dict/pyim-bigdict.pyim.gz" lye-emacs-share-dir)
  "Store the location of the pyim-dictionary.")

(defconst lye-emacs-plantuml-file
  (expand-file-name "plantuml/plantuml.jar" lye-emacs-share-dir)
  "Store the location of the plantuml.jar.")

(defconst lye-emacs-yas-snippets-dir
  (expand-file-name "snippets" lye-emacs-share-dir)
  "Store the location of the `Yas-snippets'.")

(defconst lye-emacs-custom-temp-file
  (expand-file-name "custom-template.el" lye-emacs-share-dir)
  "The custom template of `custom-file'.")

;;; customization
(defcustom lye-full-name "shanyouli"
  "Set user full name."
  :type 'string)

(defcustom lye-mail-address "shanyouli6@gmail.com"
  "Set user mail address."
  :type 'string)

(defconst lye-homepage  "https://github.com/shanyouli/emacs.d"
  "The Github page of My Emacs Configurations.")

(defcustom lye-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice (const :tag "Melpa" melpa)
                 (const :tag "Melpa-mirror" melpa-mirror)
                 (const :tag "Emacs-china" emacs-china)
                 (const :tag "Netease" netease)
                 (const :tag "Tuna" tuna)
                 (const :tag "Tencent" tencent)))

(defcustom lye-use-fuz-or-flx-in-ivy nil
  "If it is `flx', use fuzzy match with `flx' package.
If it is `fuz', use fuzzy match with `fuz' package.
If it is `nil', Not use fuzzy match."
  :type '(choice (const :tag "fuzzy match" 'flx)
                 (const :tag "fuzzy" 'fuz)
                 (const :tag "Null" nil)))

(defcustom lye-enable-sdcv-or-youdao 'sdcv
  "If it is sdcv, use `sdcv' as a translation tool.
If it is youdao, use `youdao-dictionary' as a translation tool."
  :type '(choice (const :tag "Sdcv" sdcv)
                 (const :tag "Youdao" youdao)))

(defcustom lye-company-enable-yas nil
  "Enable yasnippet for company backends or not."
  :type  'boolean)

(defcustom lye-enable-benchmark-p nil
  "Enable the init benchmark or not."
  :type 'boolean)

(defcustom lye-init-fullscreen-p nil
  "Full SCREEN or not when initializing."
  :type 'boolean)

(defcustom lye-load-all-module-file-p nil
  "Import all el files in lye-emacs-modules-dir on first run."
  :type 'boolean)

(defcustom lye-sdcv-dictionary-data-dir
  (let ((file (expand-file-name "stardict" lye-emacs-share-dir)))
    (if (file-directory-p file)
        file
      (expand-file-name "~/.stardict/")))
  "Sdcv dictionary storage directory."
  :type 'string)

(defcustom lye-emacs-autoload-dir (expand-file-name "autoload/" lye-emacs-cache-dir)
  "Automatic generation autoload file storage directory."
  :type 'string)

(defcustom lye-emacs-autoload-file (expand-file-name "loadfs.el" lye-emacs-cache-dir)
  "Extract the autoload magic annotation file from the third party package."
  :type '(choice (string :tag "Fire Name")
                 (const :tag "Error" nil)))

;; Set the temporal directory
(unless (file-exists-p lye-emacs-cache-dir)
  (make-directory lye-emacs-cache-dir))

;; make-directory lye-emacs-autoload-dir
(unless (file-exists-p lye-emacs-autoload-dir)
  (make-directory lye-emacs-autoload-dir))

;;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" lye-emacs-cache-dir))

(if (and (file-exists-p lye-emacs-custom-temp-file)
         (not (file-exists-p custom-file)))
    (copy-file lye-emacs-custom-temp-file custom-file))

(if (file-exists-p custom-file) (load custom-file))
;; -----------------------------------------------------------------------------

;; modules
(defmacro lye/modules-require (pkg)
  "Import the *.el file in the lye-emacs-modules-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-modules-dir ,pkg)))

;; init Dired
(defmacro lye/init-require (pkg)
  "Import the `*.el' file in the lye-emacs-lisp-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-init-dir ,pkg)))

;;; `Load-path'
(defun lye/add-subdidrs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT_DIR to `load-path'."
  (when (and parent-dir (file-directory-p parent-dir))
    (let* ((default-directory parent-dir))
      (setq load-path
            (append
             (cl-loop for dir in (cl-remove-if
                               (lambda (f) (string-prefix-p "." f))
                               (directory-files parent-dir))
                   unless (not (file-directory-p dir))
                   collecting (expand-file-name dir))
             load-path)))))

(defun add-to-load-path (dir-path)
  "If the `DIR-PATH' exists adding `load-path'"
  (if (file-directory-p dir-path)
      (push dir-path load-path)))

(defun lye/update-load-path (&rest _)
  "Update `load-path'."

  (mapc 'add-to-load-path
        `(
          ,lye-emacs-core-dir      ; add `lye-emacs-core-dir' to load-path
          ,lye-emacs-init-dir      ; add `lye-emacs-init-dir' to load-path
          ,lye-emacs-modules-dir   ; add `lye-emacs-modules-dir' to load-path
          ))

  ;; add `lye-emacs-site-lisp-dir' to load-path
  (lye/core-require 'core-site-lisp))

(advice-add #'package-initialize :after #'lye/update-load-path)

(lye/update-load-path)

;;; bechmark-init
(when t ;;lye-enable-benchmark-p
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate))

;;; Add after-load-theme-hook
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :after #'run-after-load-theme-hook)
;; -----------------------------------------------------------------------------

;;; Add lye-init-hook
(defvar lye-init-hook nil
  "HOOK runs after after-start-hook.")

(defun run-lye-init-hook (&rest _)
  "Run `lye-init-hook'."
  (run-at-time 0.1 nil (lambda () (run-hooks 'lye-init-hook))))

(add-hook 'after-init-hook #'run-lye-init-hook)
;; -----------------------------------------------------------------------------

(provide 'core)

;;; core.el ends here
