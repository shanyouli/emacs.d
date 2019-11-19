;;; lex-thing-edit.el --- Thing-edit shortcut key settings -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (thing-edit)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: copy, cut, replace


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

;; copy , cut, replace

;;; Code:
(require 'thing-edit)

(defonekey thing-edit nil
  "Thing-edit"
  ("w" thing-copy-word "Copy word")
  ("s" thing-copy-symbol "Copy symbol")
  ("f" thing-copy-filename "Copy-filename")
  ("x" thing-copy-sexp "Copy sexp")
  ("g" thing-copy-page "Copy Page")
  ("i" thing-copy-list "copy list")
  ("h" thing-copy-defun "copy functions")
  ("p" thing-copy-parentheses "Copy Parentheses")
  ("l" thing-copy-region-or-line "Copy line or region")
  ("a" thing-copy-to-line-beginning "Copy to Line begin")
  ("e" thing-copy-to-line-end "Copy to Line End")

  ("W" thing-cut-word "Cut word")
  ("S" thing-cut-symbol "Cut symbol")
  ("F" thing-cut-filename "Cut-filename")
  ("X" thing-cut-sexp "Cut sexp")
  ("G" thing-cut-page "Cut Page")
  ("I" thing-cut-list "Cut list")
  ("H" thing-cut-defun "Cut functions")
  ("P" thing-cut-parentheses "Cut Parentheses")
  ("L" thing-cut-region-or-line "Cut line or region")
  ("A" thing-cut-to-line-beginning "Cut to Line begin")
  ("E" thing-cut-to-line-end "Cut to Line End"))

(provide 'lex-thing-edit)

;;; lex-thing-edit.el ends here
