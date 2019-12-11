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
(require 'lib-key)

(setq lib-key-prefix "C-,")

(lib-key-unset-global "C-z" "C-SPC" "C-\\" "C-x s" "C-r" "C-x C-SPC")

;; esup
(lib-key-set-global "s e" #'esup t "esup")

;; restart-emacs
(lib-key-set-global "s r" #'restart-emacs t "restart-emacs")

;; Chinese automatically translated as English
(lib-key-set-globals
 '(("i o" . insert-translated-name-insert-original-translation)
   ("i u" . insert-translated-name-insert-with-underline)
   ("i l" . insert-translated-name-insert-with-line)
   ("i c" . insert-translated-name-insert-with-camel))
 t "lex-insert-translated-name")

;; English word completion with Chinese comments
(lib-key-set-global "i t" #'toggle-company-english-helper t "company-english-helper")

;; lex-one-key.el
(lib-key-set-globals '(("u b" . hydra-ui-menu/body)
                     ("o d" . hydra-open-dir-menu/body))
                   t "lex-hydra")

;; lex-setup-font
(when (and (display-graphic-p) (fboundp 'mdf/monospace-font-initialize+))
  (lib-key-set-global "u F" 'one-key-change-fontsize/menu t "lex-setup-font"))

;; lex-sdcv
(if (executable-find "sdcv")
    (lib-key-set-global "C-c y" 'sdcv-search-at-point++ nil "iex-sdcv")
  (lib-key-set-global "C-c y" 'youdao-dictionary-search-at-point++ nil "iex-ydcv"))

;; lex-search.el
(lib-key-set-global "C-c s" 'one-key-color-rg-search/menu nil "lex-search")

;; lex-thing-edit.el
(lib-key-set-global "M-e"  'one-key-thing-edit/menu nil "lex-thing-edit")

;; lex-pyim.el
(lib-key-set-globals
 '(("<f9>"    . toggle-default-pyim-input-method)
   ("C-<f9>"  . lye/toggle-pyim-punctuation-translate))
 nil "lex-pyim")

;; lex-funcs
(lib-key-set-global "C-z f" 'hydra-functions-menu/body nil "lex-funcs")

;; md-tmp-ext
(lib-key-set-global "o t" 'one-key-tmp-scratch/menu t "md-tmp-ext")

;; awesome-tab.el
(lib-key-set-global "C-z j" 'awesome-tab-ace-jump nil "awesome-tab")

;; lex-smex
(lib-key-set-globals
 '(("M-x" . smex)
   ("C-x M-x" . smex-major-mode-commands)) nil "lex-ido")

;;; toolkit
(lib-key-set-globals
 '(("C-x SPC" . set-mark-command)    ; Instead C-SPC for Chinese input method
   ("C-x C-h" . rectangle-mark-mode) ; rectangle-mark-mode
   ("C-z c"   . shell-command)       ; I don't know why the `M-!' in awesomewm can't be used.
   ("C-z C-z" . suspend-frame)       ; Suspend-frame
   ))



;; iex-ivy.el
(lib-key-set-globals
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
(lib-key-set-globals '(("C-z w" . elfeed)) nil "iex-elfeed")

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
(lib-key-set-globals '(("C-x g" . one-key-magit/menu)) nil "iex-git")

;; iex-window
(lib-key-set-globals '(("C-x 4 u" . winner-undo)
                 ("C-x 4 r" . winner-redo)) nil "iex-window")

;; iex-avy
(lib-key-set-globals '(("M-s" . one-key-avy/menu)) nil "iex-avy")

;; iex-vterm
(lib-key-set-globals '(("<f5>" . shell-pop)) nil "iex-term")

;; iex-pomidor.el
(lib-key-set-globals '(("C-z s c" . pomidor)) nil "iex-pomidor")

;; open line in browser
;; see @https://github.com/noctuid/link-hint.el/
(lib-key-set-globals
 '(("C-x p o" . link-hint-open-link)
   ("C-x p c" . link-hint-copy-link))
 nil "link-hint")

(with-eval-after-load 'org
  (lib-key-set-locals '(("C-x p i" . org-cliplink)) org-mode-map :file "org-cliplink"))

;; lex-snails
(when (and (not IS-WINDOWS) (display-graphic-p))
  (lib-key-unset-global "C-x C-b")
  (lib-key-set-globals '(("C-x C-b" . snails)
                        ("C-z C-s" . snails-load-theme))  nil "iex-snails"))
;; iex-tldr
(unless IS-WINDOWS
  (lib-key-set-globals '(("C-z s h" . tldr))  nil "iex-tldr"))

;; iex-smart-align
(lib-key-set-globals '(("C-z s m" . smart-align)) nil "iex-smart-align")

;; cnfonts
(setq cnfonts-directory (concat lye-emacs-cache-dir "cnfonts"))
(lib-key-set-global "C-, u f" 'cnfonts-ui nil "cnfonts")

(provide 'core-key)

;;; core-key.el ends here
