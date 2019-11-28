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
(lye/core-require 'modules-key t)

(setq md-key-prefix "C-,")

(md-key/unset-global+ "C-z" "C-SPC" "C-\\" "C-x s" "C-r" "C-x C-SPC")

;; esup
(md-key/set-ext "s e" #'esup nil t "esup")

;; restart-emacs
(md-key/set-ext "s r" #'restart-emacs nil t "restart-emacs")

;; Chinese automatically translated as English
(md-key/set-global
 '(("i o" . insert-translated-name-insert-original-translation)
   ("i u" . insert-translated-name-insert-with-underline)
   ("i l" . insert-translated-name-insert-with-line)
   ("i c" . insert-translated-name-insert-with-camel))
 t "lex-insert-translated-name")

;; English word completion with Chinese comments
(md-key/set-ext "i t" #'toggle-company-english-helper nil t "company-english-helper")

;; lex-one-key.el
(md-key/set-global '(("u b" . hydra-ui-menu/body)
                     ("o d" . hydra-open-dir-menu/body))
                   t "lex-hydra")

;; lex-setup-font
(when (and (display-graphic-p) (fboundp 'mdf/monospace-font-initialize+))
  (md-key/set-ext "u F" 'one-key-change-fontsize/menu nil t "lex-setup-font"))

;; lex-sdcv
(if (executable-find "sdcv")
    (md-key/set-ext "C-c y" 'sdcv-search-at-point++ nil nil
                  (expand-file-name "apps/sdcv/config.el" lye-modules-dir))
  (md-key/set-ext "C-c y" 'youdao-dictionary-search-at-point++ nil nil
                (expand-file-name "apps/ydcv/config.el" lye-modules-dir)))

;; lex-search.el
(md-key/set-ext "C-c s" 'one-key-color-rg-search/menu nil nil "lex-search")

;; lex-thing-edit.el
(md-key/set-ext "M-e"  'one-key-thing-edit/menu nil nil "lex-thing-edit")

;; lex-pyim.el
(md-key/set-global
 '(("<f9>"    . toggle-default-pyim-input-method)
   ("C-<f9>"  . lye/toggle-pyim-punctuation-translate))
 nil "lex-pyim")

;; lex-funcs
(md-key/set-ext "C-z f" 'hydra-functions-menu/body nil nil  "lex-funcs")

;; md-tmp-ext
(md-key/set-ext "o t" 'one-key-tmp-scratch/menu nil t "md-tmp-ext")

;; awesome-tab.el
(md-key/set-ext "C-z j" 'awesome-tab-ace-jump nil nil "awesome-tab")

;; lex-smex
(md-key/set-global
 '(("M-x" . smex)
   ("C-x M-x" . smex-major-mode-commands)) nil "lex-ido")

;;; toolkit
(md-key/set-global
 '(("C-x SPC" . set-mark-command)    ; Instead C-SPC for Chinese input method
   ("C-x C-h" . rectangle-mark-mode) ; rectangle-mark-mode
   ("C-z c"   . shell-command)       ; I don't know why the `M-!' in awesomewm can't be used.
   ("C-z C-z" . suspend-frame)       ; Suspend-frame
   ))



;; iex-ivy.el
(md-key/set-global
 '(("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x f"   . counsel-recentf)
   ("C-s"     . swiper-isearch)
   ("C-z s t" . counsel-load-theme)
   ("M-y"     . counsel-yank-pop)
   ("C-x b"   . ivy-switch-buffer)
   ("C-x d"   . counsel-dired))
 nil "iex-ivy")

;; iex-elfeed
(md-key/set-global '(("C-z w" . elfeed)) nil "iex-elfeed")

;; iex-git
;; transient file
(setq-default transient-history-file
              (concat lye-emacs-cache-dir "transient/history.el"))
(setq transient-values-file
      (concat lye-emacs-cache-dir "transient/values.el"))
(setq transient-levels-file
      (concat lye-emacs-cache-dir "transient/levels.el"))
;; Forge configuration
(setq forge-database-file
      (expand-file-name "forge-database.sqlite" lye-emacs-cache-dir))
(md-key/set-global '(("C-x g" . one-key-magit/menu)) nil "iex-git")

;; iex-window
(md-key/set-global '(("C-x 4 u" . winner-undo)
                 ("C-x 4 r" . winner-redo)) nil "iex-window")

;; iex-avy
(md-key/set-global '(("M-s" . one-key-avy/menu)) nil "iex-avy")

;; iex-vterm
(md-key/set-global '(("C-x s v" . term-toggle)) nil "iex-term")

;; iex-pomidor.el
(md-key/set-global '(("C-z s c" . pomidor)) nil "iex-pomidor")

;; open line in browser
;; see @https://github.com/noctuid/link-hint.el/
(md-key/set-global
 '(("C-x p o" . link-hint-open-link)
   ("C-x p c" . link-hint-copy-link))
 nil "link-hint")
(with-eval-after-load 'org
  (md-key/set-local '(("C-x p i" . org-cliplink)) org-mode-map :file "org-cliplink"))

;; lex-snails
(when (and (not IS-WINDOWS) (display-graphic-p))
  (md-key/unset-global+ "C-x C-b")
  (md-key/set-global '(("C-x C-b" . snails)
                   ("C-z C-s" . snails-load-theme))  nil "iex-snails"))
;; iex-tldr
(unless IS-WINDOWS
  (md-key/set-global '(("C-z s h" . tldr))  nil "iex-tldr"))

;; iex-smart-align
(md-key/set-global '(("C-z s m" . smart-align)) nil "iex-smart-align")

(provide 'core-key)

;;; core-key.el ends here
