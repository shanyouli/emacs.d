;;; lex-setup-font.el --- Setup-Font Shotcut -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (hydra)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: setup-font


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

;; Font size adjustment

;;; Code:

;; Font size adjustment
(defhydra hydra-font-size-menu (:exit nil)
  "Font size"
  ("=" increase-setup-font-size "Inc font size")
  ("-" decrease-setup-font-size "Dec font size")
  ("0" default-setup-font-size "Default font size" :exit t)
  ("q" nil "quit"))

(defonekey change-fontsize t
  "Font size."
  ("=" mdf/increase-font-size "Inc font size")
  ("-" mdf/decrease-font-size "Dec font size")
  ("0" mdf/goto-default-size-font "Default font size"))

(provide 'lex-setup-font)

;;; lex-setup-font.el ends here
