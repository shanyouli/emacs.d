;;; iex-avy.el --- Initialize Avy -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1.0
;; Package-Requires: (avy)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: goto-char


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

;; a fast positioning tool

;;; Code:

(package! 'avy t t)
(package! 'ace-pinyin t t)

;;; configurations
(setq avy-background 1)

;; key
(defvar one-key-menu-avy-alist nil
  "The `one-key' menu alist for AVY.")

(setq one-key-menu-avy-alist
      '((("c"   . "Goto Char") . avy-goto-char)
        (("v"   . "Goto Char 2") . avy-goto-char-2)
        (("w"   . "Goto word") . avy-goto-word-0)
        (("e"   . "Goto Word 1") . avy-goto-word-1)
        (("l"   . "Goto Begin Line") . avy-goto-line)
        (("L"   . "Goto end Line") . avy-goto-end-of-line)
        (("SPC" . "Goto char in line") . avy-goto-char-in-line)))

(defun one-key-menu-avy ()
  "The `one-key' menu for AVY."
  (interactive)
  (one-key-menu "AVY" one-key-menu-avy-alist t))


;; run
(avy-setup-default)
(ace-pinyin-global-mode t)

(provide 'iex-avy)

;;; iex-avy.el ends here
