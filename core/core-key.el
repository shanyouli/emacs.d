;;; core-key.el --- Init some packages -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (eusp restart-emacs lazy-load )
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: lazy-load


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

;; Init some packages

;;; Code:

(require 'lazy-load)
;;(require 'one-key)

;;Global uninstall button
(lazy-load-unset-keys '("C-z" "C-SPC" "C-\\" "C-x s"))

;; esup
(lazy-load-global-keys '(("C-z e" . esup)) "esup")

;; restart-emacs
(lazy-load-global-keys '(("C-z r" . restart-emacs)) "restart-emacs")

;; aweshell
(lazy-load-global-keys
 '(
   ("C-x s n" . aweshell-new)
   ("C-x s a" . aweshell-toggle)
   ("C-x s d" . aweshell-dedicated-toggle)
   )
 "aweshell")

;; multi-term
(lazy-load-global-keys
 '(
   ("C-x s t" . multi-term-dedicated-toggle)
   ("C-x s m" . multi-term)
   )
 "init-multi-term")

;; I don't know why the shortcut key (M-!) in awesomewm can't be used.
(lazy-load-global-keys '(("C-z c" . shell-command)) "simple")

;; Setup change size font, base on emacs-font-size  pair-list
(when (display-graphic-p)
  (lazy-load-global-keys '(("C-z =" . increase-emacs-font-size)
                           ("C-z -" . decrease-emacs-font-size)
                           ("C-z 0" . default-emacs-font-size))
                         "core-font"))

(provide 'core-key)

;;; core-key.el ends here
