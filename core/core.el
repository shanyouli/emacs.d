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
(eval-when-compile (require 'cl))

;; constant

(defconst system/windows (eq system-type 'windows-nt)
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

(defcustom lye-package-archives 'tuna
  "Set package archives from which to fetch."
  :type '(choice (const :tag "Melpa" melpa)
                 (const :tag "Melpa-mirror" melpa-mirror)
                 (const :tag "Emacs-china" emacs-china)
                 (const :tag "Netease" netease)
                 (const :tag "Tuna" tuna)
                 (const :tag "Tencent" tencent)))

(defcustom lye-company-enable-yas nil
  "Enable yasnippet for company backends or not."
  :type  'boolean)

(defcustom lye-enable-benchmark-p nil
  "Enable the init benchmark or not."
  :type 'boolean)

(defcustom lye-init-fullscreen-p nil
  "Full SCREEN or not when initializing."
  :type 'boolean)

(defcustom lye-use-scratch-p nil
  "Whether to use `*scratch*' buffer"
  :type 'boolean)
(defcustom lye-load-all-module-file-p nil
  "Import all el files in lye-emacs-modules-dir on first run."
  :type 'boolean)

(defcustom lye-use-amx-or-fuzzy 'amx
  "Because when using the `counsel-M-x', the amx and fuz packages conflict,
only the fuz is used, not both. If it is 'fuzzy, Only use fuzzy match.
If it is 'amx, Only use amx and not use fuzzy match."
  :type '(choice (const :tag "fuzzy match" 'fuzzy)
                 (const :tag "amx" 'amx)))

(defcustom lye-enable-sdcv-or-youdao 'sdcv
  "If it is sdcv, use `sdcv' as a translation tool.
If it is youdao, use `youdao-dictionary' as a translation tool."
  :type '(choice (const :tag "Sdcv" sdcv)
                 (const :tag "Youdao" youdao)))

(defcustom lye-sdcv-dictionary-data-dir (expand-file-name "stardict" lye-emacs-share-dir)
  "Sdcv dictionary storage directory."
  :type 'string)

;;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" lye-emacs-cache-dir))

(if (and (file-exists-p lye-emacs-custom-temp-file)
         (not (file-exists-p custom-file)))
    (copy-file lye-emacs-custom-temp-file custom-file))

(if (file-exists-p custom-file) (load custom-file))

;; Set the temporal directory
(unless (file-exists-p lye-emacs-cache-dir)
  (make-directory lye-emacs-cache-dir))

;;; `Load-path'
(defun lye/add-subdidrs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT_DIR to `load-path'."
  (when (and parent-dir (file-directory-p parent-dir))
    (let* ((default-directory parent-dir))
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (or (not (file-directory-p dir))
                              (string= dir ".")
                              (string= dir ".."))
                   collecting (expand-file-name dir))
             load-path)))))

(defun lye/update-load-path (&rest _)
  "Update `load-path'."
  ;; add `lye-emacs-init-dir' to load-path
  (push lye-emacs-init-dir load-path)

  ;; add `lye-emacs-site-lisp-dir' to load-path
  (lye/add-subdidrs-to-load-path lye-emacs-site-lisp-dir)

  ;; add `lye-emacs-modules-dir' to load-path
  (push lye-emacs-modules-dir load-path))

(advice-add #'package-initialize :after #'lye/update-load-path)

(lye/update-load-path)

;;; bechmark-init
(when lye-enable-benchmark-p
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate))

;; modules
(defmacro lye/modules-require (pkg)
  "Import the *.el file in the lye-emacs-modules-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-modules-dir ,pkg)))

;; init Dired
(defmacro lye/init-require (pkg)
  "Import the `*.el' file in the lye-emacs-lisp-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-init-dir ,pkg)))

(provide 'core)

;;; core.el ends here
