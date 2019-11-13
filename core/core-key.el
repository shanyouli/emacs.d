;;; core-key.el --- Init some packages -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (eusp restart-emacs)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: Keybindings


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

;; Global uninstall button
(md-key/unset-keys+ '("C-z" "C-SPC" "C-\\" "C-x s" "C-r" "C-x C-SPC"))

(setq md-key-prefix "C-,")

;; esup
(mdk/set-key! "s e" #'esup nil t "esup")

;; restart-emacs
(mdk/set-key! "s r" #'restart-emacs nil t "restart-emacs")

;; Chinese automatically translated as English
(mdk/set-keys!
 '(
   ("i o" . insert-translated-name-insert-original-translation)
   ("i u" . insert-translated-name-insert-with-underline)
   ("i l" . insert-translated-name-insert-with-line)
   ("i c" . insert-translated-name-insert-with-camel))
 nil t "lex-insert-translated-name")

;; English word completion with Chinese comments
(mdk/set-key! "i t" #'toggle-company-english-helper nil t "company-english-helper")

;; lex-one-key.el
(mdk/set-keys! '(("u b" . hydra-ui-menu/body)
                 ("o d" . hydra-open-dir-menu/body))
               nil t "lex-hydra")

;; lex-setup-font
(when (and (display-graphic-p) (fboundp 'setup-font-initialize))
  (mdk/set-key! "u F" 'one-key-change-fontsize/menu nil t "lex-setup-font"))

;; lex-sdcv
(if (and (executable-find "sdcv") (eq lye-enable-sdcv-or-youdao 'sdcv))
    (mdk/set-key! "C-, y" 'sdcv-search-at-point++ nil nil "lex-sdcv")
  (setq lye-enable-sdcv-or-youdao 'youdao))

;; lex-search.el
(mdk/set-key! "C-r" 'one-key-menu-search nil nil "lex-search")

;; lex-thing-edit.el
(mdk/set-key! "M-e"  'one-key-menu-thing-edit nil nil "lex-thing-edit")

;; lex-pyim.el
(mdk/set-keys!
 '(("<f9>"    . toggle-default-pyim-input-method)
   ("C-<f9>"  . lye/toggle-pyim-punctuation-translate))
 nil nil "lex-pyim")

;; lex-funcs
(mdk/set-key! "C-z f" 'hydra-functions-menu/body nil nil  "lex-funcs")

;; lex-temp
(mdk/set-key! "C-z b" 'hydra-tmp-scratch-menu/body nil nil "lex-temp")

;; awesome-tab.el
(mdk/set-key! "C-z j" 'awesome-tab-ace-jump nil nil "awesome-tab")

;; lex-smex
(mdk/set-keys!
 '(("M-x" . smex)
   ("C-x M-x" . smex-major-mode-commands)) nil nil "lex-ido")

;;; toolkit
(mdk/set-keys!
 '(("C-x SPC" . set-mark-command)    ; Instead C-SPC for Chinese input method
   ("C-x C-h" . rectangle-mark-mode) ; rectangle-mark-mode
   ("C-z c"   . shell-command)       ; I don't know why the `M-!' in awesomewm can't be used.
   ("C-z C-z" . suspend-frame)       ; Suspend-frame
   ))

(provide 'core-key)

;;; core-key.el ends here
