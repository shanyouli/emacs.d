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

(defvar one-key-menu-thing-edit-alist nil
  "The `one-key' menu alist for THING-EDIT")

(setq one-key-menu-thing-edit-alist
      '(;; Copy
        (("w" . "Copy word")           . thing-copy-word)
        (("s" . "Copy Symbol")         . thing-copy-symbol)
        (("m" . "Copy Email")          . thing-copy-email)
        (("f" . "Copy Filename")       . thing-copy-filename)
        (("u" . "Copy URL")            . thing-copy-url)
        (("x" . "Copy Sexp")           . thing-copy-sexp)
        (("g" . "Copy Page")           . thing-copy-page)
        (("t" . "Copy Sentence")       . thing-copy-sentence)
        (("o" . "Copy Whitespace")     . thing-copy-whitespace)
        (("i" . "Copy List")           . thing-cop-list)
        (("h" . "Copy Functions")      . thing-copy-defun)
        (("p" . "Copy Parentheses")    . thing-copy-parentheses)
        (("l" . "Copy line or region") . thing-copy-region-or-line)
        (("a" . "Copy to Line Begin")  . thing-copy-to-line-beginning)
        (("e" . "Copy to Line End")    . thing-copy-to-line-end)

        ;;Cut
        (("W" . "Cut word")            . thing-cut-word)
        (("S" . "Cut Symbol")          . thing-cut-symbol)
        (("M" . "Cut Email")           . thing-cut-email)
        (("F" . "Cut Filename")        . thing-cut-filename)
        (("U" . "Cut URL")             . thing-cut-url)
        (("X" . "Cut Sexp")            . thing-cut-sexp)
        (("G" . "Cut Page")            . thing-cut-page)
        (("T" . "Cut Sentence")        . thing-cut-sentence)
        (("O" . "Cut Whitespace")      . thing-cut-whitespace)
        (("I" . "Cut List")            . thing-cop-list)
        (("H" . "Cut Functions")       . thing-cut-defun)
        (("P" . "Cut Parentheses")     . thing-cut-parentheses)
        (("L" . "Cut line or region")  . thing-cut-region-or-line)
        (("A" . "Cut to Line Begin")   . thing-cut-to-line-beginning)
        (("E" . "Cut to Line End")     . thing-cut-to-line-end)
        ))

(defun one-key-menu-thing-edit ()
  "The `one-key' menu for THING-EDIT."
  (interactive)
  (one-key-menu "THING-EDIT" one-key-menu-thing-edit-alist t))

(provide 'lex-thing-edit)

;;; lex-thing-edit.el ends here
