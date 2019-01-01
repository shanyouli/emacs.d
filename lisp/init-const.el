;;; init-const.el --- Define constants.              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
;; Keywords:

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

;; Define constants.

;;; Code:

(defconst lye-homepage
  "https://github.com/lye95/emacs.d"
  "The Github pae of Lye Emacs.")

(defconst *is-a-win*
  (eq system-type 'windows-nt)
  "Are we running on a Windows System?")

(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Are we running on a Mac System?")

(defconst *is-a-linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux System?")

(defconst *root*
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

;; x w32 pc
(defconst *is-a-term*
  (eq window-system nil)
  "Are you using Emacs in a terminatal?")

;; Set the temporal directory
(defconst lye-emacs-temporal-dir
  (concat user-emacs-directory "tmp/")
  "Is the temporal diirectory this?")

(provide 'init-const)

;;; init-const.el ends here
