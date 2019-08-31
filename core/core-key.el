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

;;Global uninstall button
(lazy-load-unset-keys '("C-z" "C-SPC" "C-\\" "C-x s" "C-r"))

;; esup
(lazy-load-global-keys '(("C-z e" . esup)) "esup")

;; restart-emacs
(lazy-load-global-keys '(("C-z r" . restart-emacs)) "restart-emacs")

;; aweshell
;; (lazy-load-global-keys
;;  '(
;;    ("C-x s n" . aweshell-new)
;;    ("C-x s a" . aweshell-toggle)
;;    ("C-x s d" . aweshell-dedicated-toggle))
;;  "aweshell")

;; Chinese automatically translated as English
(lazy-load-global-keys
 '(
   ("C-z i o" . insert-translated-name-insert-original-translation)
   ("C-z i u" . insert-translated-name-insert-with-underline)
   ("C-z i l" . insert-translated-name-insert-with-line)
   ("C-z i c" . insert-translated-name-insert-with-camel))
 "lex-insert-translated-name")

;; English word completion with Chinese comments
(lazy-load-global-keys
 '(("C-z i t" . toggle-company-english-helper))
 "company-english-helper")

;; lex-one-key.el
(lazy-load-global-keys
 '(("C-z u"   . hydra-ui-menu/body)
   ("C-z d"   . hydra-open-dir-menu/body))
 "lex-hydra")

;; lex-setup-font
(when (and (display-graphic-p) (fboundp 'setup-font-initialize))
  (lazy-load-global-keys '(("C-z s f" . hydra-font-size-menu/body))
                         "lex-setup-font"))

;; lex-sdcv
(if (and (executable-find "sdcv") (eq lye-enable-sdcv-or-youdao 'sdcv))
    (lazy-load-global-keys '(("C-z y" . sdcv-search-at-point++))
                           "lex-sdcv")
  (setq lye-enable-sdcv-or-youdao 'youdao))

;; lex-search.el
(lazy-load-global-keys '(("C-r" . one-key-menu-search)) "lex-search")

;; lex-thing-edit.el
(lazy-load-global-keys '(("M-e" . one-key-menu-thing-edit)) "lex-thing-edit")

;; lex-pyim.el
(lazy-load-global-keys
 '(("<f9>"    . toggle-default-pyim-input-method)
   ("C-<f9>"  . lye/toggle-pyim-punctuation-translate))
 "lex-pyim")

;; lex-funcs
(lazy-load-global-keys '(("C-z f" . hydra-functions-menu/body)) "lex-funcs")

;; lex-temp
(lazy-load-global-keys '(("C-z b" . hydra-tmp-scratch-menu/body)) "lex-temp")

;; awesome-tab.el
(lazy-load-global-keys '(("C-z j" . awesome-tab-ace-jump)) "awesome-tab")

;; lex-smex
(lazy-load-global-keys
 '(("M-x" . smex)
   ("C-x M-x" . smex-major-mode-commands)) "lex-smex")

;;; toolkit
(lazy-load-set-keys
 '(
   ("C-x SPC" . set-mark-command)    ; Instead C-SPC for Chinese input method
   ("C-x C-h" . rectangle-mark-mode) ; rectangle-mark-mode
   ("C-z c"   . shell-command)       ; I don't know why the `M-!' in awesomewm can't be used.
   ("C-z C-z" . suspend-frame)       ; Suspend-frame
   ))


(provide 'core-key)

;;; core-key.el ends here
