;;; init-custom.el ---Custom Configurations.         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>
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

;;

;;; Code:


(defcustom lye-full-name "lye li"
  "Set user full name."
  :type 'string)

(defcustom lye-mail-address "shanyouli6@gmail.com"
  "Set user mail address."
  :type 'string)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;Set package archives from which to fetch
(defcustom lye-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa-mirror" melpa-mirror)
	  (const :tag "Emacs-china" emacs-china)
	  (const :tag "Netease" netease)))

;; Set theme
(defcustom lye-themes 'default
  "Set color theme."
  :type '(choice
	  (const :tag "Monokai Theme" default)
	  (const :tag "Tao theme" light)
	  (const :tag "Tao theme" dark)
	  ))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-custom)
;;; init-custom.el ends here
