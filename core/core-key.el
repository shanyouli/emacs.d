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

;; Mainly the shortcut keys for local and third-party packages

;;; Code:

(require 'lazy-load)
(require 'one-key)

;;Global uninstall button
(lazy-load-unset-keys '("C-z" "C-SPC" "C-\\" "C-x s" "C-r"))

;; suspend-frame
(lazy-load-global-keys '(("C-z C-z" . suspend-frame)) "frame")

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
   ("C-x s t" . multi-term-dedicated-toggle+)
   ("C-x s m" . multi-term)
   )
 "lex-multi-term")

;; Setup change size font, base on emacs-font-size  pair-list
(when (display-graphic-p)
  (lazy-load-global-keys '(("C-z f" . lye/one-key-menu-font-size)) "core-font"))

;; Chinese automatically translated as English
(lazy-load-global-keys
 '(
   ("C-z i o" . insert-translated-name-insert-original-translation)
   ("C-z i u" . insert-translated-name-insert-with-underline)
   ("C-z i l" . insert-translated-name-insert-with-line)
   ("C-z i c" . insert-translated-name-insert-with-camel)
   )
 "insert-translated-name")

;; English word completion with Chinese comments
(lazy-load-global-keys
 '(("C-z i t" . toggle-company-english-helper))
 "company-english-helper")

;; lex-one-key.el
(lazy-load-global-keys '(("C-z u" . one-key-menu-ui)) "lex-one-key")

;; lex-translate.el
(lazy-load-global-keys '(("C-z y" . one-key-menu-translate)) "lex-translate")

;; lex-search.el
(lazy-load-global-keys '(("C-r" . one-key-menu-search)) "lex-search")

;;; Toolkit
(lazy-load-set-keys
 '(
   ("M-h" . set-mark-command) ; Instead C-SPC for Chinese input method
   ("C-z c" . shell-command)  ; I don't know why the `M-!' in awesomewm can't be used.
   ))

(provide 'core-key)

;;; core-key.el ends here
