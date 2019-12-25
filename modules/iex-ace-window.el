;;; iex-ace-window.el --- Initialize Ace-Window -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (ace)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: window-manager
;; Last-Updated: 2019-12-04 20:00:39


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

;;

;;; Change log:
;;
;; 12/04/19

;;; Code:
(require 'ace-window)

(custom-set-faces '(aw-mode-line-face
                    ((t (:inherit mode-line-emphasis :bold t)))))
(custom-set-faces '(aw-leading-char-face
                    ((t (:inherit font-lock-keyword-face :bold t :height 2.5)))))

(ace-window-display-mode)

(provide 'iex-ace-window)

;;; iex-ace-window.el ends here
