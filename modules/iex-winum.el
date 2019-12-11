;;; iex-winum.el --- Initialize Winum -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (winum)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: winum
;; Last-Updated: 2019-12-04 20:02:11


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

;; commentary

;;; Change log:
;;
;; 12/04/19

;;; Code:

(require 'winum)
(winum-mode)
(lib-key-set-locals
 '(("M-0" . winum-select-window-0-or-10)
   ("M-1" . winum-select-window-1)
   ("M-2" . winum-select-window-2)
   ("M-3" . winum-select-window-3)
   ("M-4" . winum-select-window-4))
 winum-keymap)

(provide 'iex-winum)

;;; iex-winum.el ends here
