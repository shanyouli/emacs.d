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

(package! 'avy t)
(package! 'ace-pinyin t)
(require 'avy)
(require 'ace-pinyin)

;;; configurations
(setq avy-background 1)

;; key
(defonekey avy nil
  "Font size."
  ("c" avy-goto-char "Goto char")
  ("v" avy-goto-char-2 "Goto Char 2")
  ("w" avy-goto-word-0 "Got Word")
  ("e" avy-goto-word-1 "Goto word 1")
  ("l" avy-goto-line "Goto Begin Line")
  ("L" avy-goto-end-of-line "Goto End Line")
  ("SPC" avy-goto-char-in-line "Goto char inline"))

;; run
(avy-setup-default)
(ace-pinyin-global-mode +1)

(provide 'iex-avy)

;;; iex-avy.el ends here
