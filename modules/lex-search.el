;;; lex-search.el --- Initialize Search -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (lazy-search color-rg)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: search


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

;; Search packages configurations

;;; Code:

;; lazy-search configuration ---------------------------------------------------

(require 'lazy-search)

(custom-set-faces
 '(lazy-search-highlight-current
   ((t :foreground "black" :background "orange" :bold t)))
 '(lazy-search-highlight-background
   ((t :foreground "gray" :background "black" :bold t))))

;; Fix lazy-search and rainbow-mode conflicts
(defvar lye-rainbow-mode-active-p nil
  "`rainbow-mode' is run or not.")
(defun lye/rainbow-turn-off ()
  (when (and (featurep 'rainbow-mode) rainbow-mode)
    (setq lye-rainbow-mode-active-p t)
    (rainbow-mode -1)))
(defun lye/rainbow-turn-on ()
  (when lye-rainbow-mode-active-p
    (setq lye-rainbow-mode-active-p nil)
    (rainbow-mode 1)))
(advice-add #'lazy-search-quit :after #'lye/rainbow-turn-on)
(advice-add #'lazy-search :after  #'lye/rainbow-turn-off)
;; -----------------------------------------------------------------------------

;; color-rg configuration ------------------------------------------------------
(require 'color-rg)
;; -----------------------------------------------------------------------------

;; one-key configurations
(defonekey color-rg-search nil
  "Search"
  ("s" color-rg-search-symbol "symbol")
  ("i" color-rg-search-input "input")
  ("p" color-rg-search-symbol-in-project "symbol in proj")
  ("j" color-rg-search-input-in-project "input in proj")
  ("c" color-rg-search-symbol-in-current-file "symbol in file")
  ("v" color-rg-search-input-in-current-file "input in file"))

(provide 'lex-search)

;;; lex-search.el ends here
