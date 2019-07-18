;;; init-thing-edit.el --- Initialize thing-edit -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (pretty-hydra thing-edit)
;; Homepage: homepage
;; Keywords: yank, cut, replace


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

;;; Code:

(use-package thing-edit
  :ensure nil
  :commands (;; copy
             thing-copy-word thing-copy-symbol thing-copy-email
             thing-copy-filename thing-copy-url thing-copy-sexp thing-copy-page
             thing-copy-whitespace thing-copy-list thing-copy-comment
             thing-copy-defun thing-copy-parentheses thing-copy-line
             thing-copy-to-line-beginning thing-copy-to-line-end
             ;; cut
             thing-cut-word thing-cut-symbol thing-cut-email
             thing-cut-filename thing-cut-url thing-cut-sexp thing-cut-page
             thing-cut-whitespace thing-cut-list thing-cut-comment
             thing-cut-defun thing-cut-parentheses thing-cut-line
             thing-cut-to-line-beginning thing-cut-to-line-end
             ;; replace
             thing-replace-word thing-replace-symbol thing-replace-email
             thing-replace-filename thing-replace-url thing-replace-sexp thing-replace-page
             thing-replace-whitespace thing-replace-list thing-replace-comment
             thing-replace-defun thing-replace-parentheses thing-replace-line
             thing-replace-to-line-beginning thing-replace-to-line-end
             )
  :pretty-hydra
  ((:title (pretty-hydra-title "Thing Edit" 'material "edit")
    :color blue :quit-key "q")
   ("Copy"
    (("w" thing-copy-word "Copy Word")
     ("s" thing-copy-symbol "Copy Symbol")
     ("m" thing-copy-email "Copy Email")
     ("f" thing-copy-filename "Copy Filename")
     ("u" thing-copy-url "Copy URL")
     ("x" thing-copy-sexp "Copy Sexp")
     ("g" thing-copy-page "Copy Page")
     ("t" thing-copy-sentence "Copy Sentence")
     ("o" thing-copy-whitespace "Copy Whitespace")
     ("i" thing-copy-list "Copy List")
     ("h" thing-copy-defun "Copy Functions")
     ("p" thing-copy-parentheses "Copy Parentheses")
     ("l" thing-copy-line "Copy Line")
     ("a" thing-copy-to-line-beginning "Copy to Line Begin")
     ("e" thing-copy-to-line-end "Copy to line end"))
    "Cut"
    (("W" thing-cut-word "Cut Word")
     ("S" thing-cut-symbol "Cut Symbol")
     ("M" thing-cut-email "Cut Email")
     ("F" thing-cut-filename "Cut Filename")
     ("U" thing-cut-url "Cut URL")
     ("X" thing-cut-sexp "Cut Sexp")
     ("G" thing-cut-page "Cut Page")
     ("T" thing-cut-sentence "Cut Sentence")
     ("O" thing-cut-whitespace "Cut Whitespace")
     ("I" thing-cut-list "Cut List")
     ("H" thing-cut-defun "Cut Functions")
     ("P" thing-cut-parentheses "Cut Parentheses")
     ("L" thing-cut-line "Cut Line")
     ("A" thing-cut-to-line-beginning "Cut to Line Begin")
     ("E" thing-cut-to-line-end "Cut to line end"))
    "Replace"
    (("r" replace-regexp "Replace regexp")
     ("R" replace-string "Replace string")
     ("1" thing-replace-word "Replace word")
     ("2" replace-rectangle "Replace rectangle"))
    "Mark"
    (("SPC" set-mark-command "Mark")
     ("C-s" mark-sexp "Mark sexp")
     ("C-w" mark-word "Mark word")
     ("C-f" mark-defun "Mark a functions")
     ("C-a" mark-whole-buffer "Mark the buffer")
     ("C-p" mark-page "Mark page"))))
  :bind ("C-k" . thing-edit-hydra/body))

(provide 'init-thing-edit)

;;; init-thing-edit.el ends here
