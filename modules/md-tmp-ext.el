;;; md-tmp-ext.el --- Keybindings in `modules-tmp-scratch.el' -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (moudles-tmp-scratch)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: scratch
;; Last-Updated: 2019-11-22 11:39:38


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

;; Initialize key-binding

;;; Change log:
;;
;; 11/22/19

;;; Code:
(require 'tmp-scratch)
(defonekey tmp-scratch nil
  "Temp file scratch"
  ("t" lib-scratch/orig "text")
  ("e" lib-scratch/el   "Elisp")
  ("p" lib-scratch/py   "Python")
  ("s" lib-scratch/sh   "Bash/zsh"))

(provide 'md-tmp-ext)

;;; md-tmp-ext.el ends here
