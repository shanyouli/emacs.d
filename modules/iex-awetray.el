;;; iex-awetray.el --- Initialize Awesome Tray -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (awesome-tray)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: modeline
;; Last-Updated: 2019-12-07 13:43:44


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

;; Initialize Awesome-Tray

;;; Change log:
;;
;; 12/07/19

;;; Code:

(require 'awesome-tray)

;; add pyim-modules
(defface awesome-tray-module-pyim-face
  '((t (:foreground "OrangeRed" :bold t)))
  "Pyim face."
  :group 'awesome-tray)

(defun awesome-tray-module-pyim-info ()
  "`Pyim' display information in the awesome-tray."
  (if (and (featurep 'pyim) (string= current-input-method "pyim"))
      "<IM>" ""))

(push '("pyim" . (awesome-tray-module-pyim-info awesome-tray-module-pyim-face))
      awesome-tray-module-alist)

;; active-modules
(setq awesome-tray-active-modules
      '("pyim" "location" "parent-dir" "mode-name" "awesome-tab" "date"))

(defun awesome-tray-initialize+ ()
  "Start `Awesome-tray'"
  (if (fboundp 'theme-switch-light-or-dark-theme)
      (advice-add #'theme-switch-light-or-dark-theme :after #'awesome-tray-mode)
    (awesome-tray-mode))
  (when (boundp 'after-load-theme-hook)
    (add-hook 'after-load-theme-hook
              '(lambda () (when (and (boundp 'awesome-tray-active-p) awesome-tray-active-p)
                       (awesome-tray-mode))))))

(awesome-tray-initialize+)

(provide 'iex-awetray)

;;; iex-awetray.el ends here
