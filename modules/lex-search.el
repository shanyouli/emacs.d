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
    ((t :foreground "black" :background "green" :bold t))))

;;(defadvice lazy-search)

(advice-add #'lazy-search-quit :after #'rainbow-turn-on)
(advice-add #'lazy-search :after  #'rainbow-turn-off)
;; -----------------------------------------------------------------------------

;; color-rg configuration ------------------------------------------------------
(require 'color-rg)
;; -----------------------------------------------------------------------------

;; one-key configurations
(defvar one-key-menu-search-alist nil
  "The `one-key' menu list for SEARCH.")

(setq one-key-menu-search-alist
      '((("g" . "Use rg search symbol") . color-rg-search-symbol)
        (("h" . "Use rg search input") . color-rg-search-input)
        (("j" . "Use rg search symbol in project") . color-rg-search-symbol-in-project)
        (("k" . "Use rg search input in project")  . color-rg-search-input-in-project)
        (("," . "Use rg search symbol in current file") . color-rg-search-symbol-in-current-file)
        (("." . "Use rg search input in current file") . color-rg-search-input-in-current-file)
        (("l" . "lazy search") . lazy-search)))

(defun one-key-menu-search ()
  "The `one-key' menu for SEARCH"
  (interactive)
  (one-key-menu "SEARCH" one-key-menu-search-alist t))

(provide 'lex-search)

;;; lex-search.el ends here
