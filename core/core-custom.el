;;; core-custom.el --- Management and variable definitions -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/.emacs.d
;; Keywords: vars, const


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

;; Management and variable definitions

;;; Code:


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

(defconst lye-emacs-core-autoload-dir (expand-file-name "autoload/" lye-emacs-core-dir)
  "autoload dir in `lye-emacs-core-dir'")

(defconst lye-emacs-core-modules-dir (expand-file-name "modules/" lye-emacs-core-dir)
  "modules dir in `lye-emacs-core-dir'")

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

(defcustom lye-init-fullscreen-p nil
  "Full SCREEN or not when initializing."
  :type 'boolean)

(defcustom lye-load-all-module-file-p nil
  "Import all el files in lye-emacs-modules-dir on first run."
  :type 'boolean)

(defcustom lye-sdcv-dictionary-data-dir
  (let ((file1 (expand-file-name "sdcv" lye-emacs-share-dir))
        (file2 (expand-file-name "sdcv" (getenv "XDG_DATA_HOME"))))
    (cond ((file-directory-p file2)
           file2)
          ((file-directory-p file1)
           file)
          (t
           (expand-file-name "~/.stardict/"))))
  "Sdcv dictionary storage directory."
  :type 'directory)

(defcustom lye-emacs-autoload-dir (expand-file-name "autoload/" lye-emacs-cache-dir)
  "Automatic generation autoload file storage directory."
  :type 'directory)

(dolist (d (list lye-emacs-cache-dir
                 lye-emacs-autoload-dir))
  (unless (file-directory-p d)
    (make-directory d t)))

;;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" lye-emacs-cache-dir))

(if (and (file-exists-p lye-emacs-custom-temp-file)
         (not (file-exists-p custom-file)))
    (copy-file lye-emacs-custom-temp-file custom-file))

(if (file-exists-p custom-file) (load custom-file))

;; https://github.com/honmaple/dotfiles/blob/571d6f0dca10015886c56a1feab17f0d5a1bb1ab/emacs.d/init.el#L51
(defmacro lye/core-require (pkg &optional modulep)
  "Load PKG. When MODULEP is non-nil, the presence of PKG using directory
`lye-emacs-core-modules-dir', and vice versa for `lye-emacs-core-dir'."
  (let ((dir (if modulep
                 lye-emacs-core-modules-dir
               lye-emacs-core-dir)))
    `(require ,pkg (expand-file-name (format "%s" ,pkg) ,dir))))

(provide 'core-custom)

;;; core-custom.el ends here
