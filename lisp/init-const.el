;;; init-const.el ---Define Constants                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:constants

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

;;

;;; Code:

;;; Constants
(defconst lye-homepage  "https://github.com/shanyouli/emacs.d"
  "The Github page of My Emacs Configurations.")

(defconst system/windows (eq system-type 'windows-nt)
  "Are we running on a Windows System?")

(defconst system/mac (eq system-type 'darwin)
  "Are we running on a Mac System?")

(defconst system/linux (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux System?")

(defconst *root* (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst lye-emacs-cache-dir (concat user-emacs-directory "cache/")
  "Is the cache directory this?")

;; (expand-file-name "site-lisp/" user-emacs-directory)
(defconst lye-emacs-site-lisp-dir (concat user-emacs-directory "site-lisp/")
  "Use the git-submodule managed plugin to store the location.")

(provide 'init-const)
;;; init-const.el ends here
