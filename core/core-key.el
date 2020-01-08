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

(lib-key-unset "C-z" "C-SPC" "C-\\" "C-x s" "C-r" "C-x C-SPC")

;; esup
(lib-key-define :autoload "esup" "C-, s e" #'esup)

;; restart-emacs
(lib-key-define "C-, s r" #'restart-emacs
                :autoload "restart-emacs")

;; Chinese automatically translated as English
(lib-key-define :autoload "lex-insert-translated-name"
                :prefix "C-,"
                "io" 'insert-translated-name-insert-original-translation
                "iu" 'insert-translated-name-insert-with-underline
                "il" 'insert-translated-name-insert-with-line
                "ic" 'insert-translated-name-insert-with-camel)

;; English word completion with Chinese comments
(lib-key-define :autoload "company-english-helper"
                "C-, it" #'toggle-company-english-helper)

;; lex-one-key.el
(lib-key-define :autoload "lex-hydra"
                "C-, ub" 'hydra-ui-menu/body
                "C-, od" 'hydra-open-dir-menu/body)

;; font
(when (display-graphic-p)
  (lib-key-define :autoload "lib-font"
                  :prefix "C-, uf"
                  "=" 'lib-font/increase-font-size
                  "-" 'lib-font/decrease-font-size
                  "9" 'lib-font/display-font-size)
  (setq cnfonts-directory (concat lye-emacs-cache-dir "cnfonts"))
  (lib-key-define "C-, ufc" 'cnfonts-ui :autoload "cnfonts"))

;; lex-sdcv
(if (executable-find "sdcv")
    (lib-key-define "C-c y" 'sdcv-search-at-point++ :autoload "iex-sdcv")
  (lib-key-define "C-c y" 'youdao-dictionary-search-at-point++ :autoload "iex-ydcv"))

;; lex-search.el
(lib-key-define "C-c s" 'one-key-color-rg-search/menu :autoload "lex-search")

;; lex-thing-edit.el
(lib-key-define "M-e"  'one-key-thing-edit/menu :autoload "lex-thing-edit")

;; lex-pyim.el
(lib-key-define "<f9>" 'toggle-default-pyim-input-method
                "C-<f9>" 'lye/toggle-pyim-punctuation-translate
                :autoload "lex-pyim")

;; lex-funcs
(lib-key-define "C-z f" 'hydra-functions-menu/body :autoload "lex-funcs")

;; md-tmp-ext
(lib-key-define "C-, ot" 'one-key-tmp-scratch/menu :autoload "md-tmp-ext")

;; awesome-tab.el
(lib-key-define "C-z j" 'awesome-tab-ace-jump :autoload "awesome-tab")

;; lex-smex
(lib-key-define "M-x"     'smex
                "C-x M-x" 'smex-major-mode-commands
                :autoload "lex-ido")

;;; toolkit
(lib-key-define "C-x SPC" 'set-mark-command    ; Instead C-SPC for Chinese input method
                "C-x C-h" 'rectangle-mark-mode ; rectangle-mark-mode
                "C-z C-z" 'suspend-frame)    ; Suspend-frame

;; iex-ivy.el
(lib-key-define "M-x"     'counsel-M-x
                "C-x C-f" 'counsel-find-file
                "C-x f"   'counsel-recentf
                "C-s"     'swiper-isearch
                "C-z s t" 'counsel-load-theme
                "M-y"     'counsel-yank-pop
                "C-x b"   'ivy-switch-buffer
                "C-x d"   'counsel-dired
                "C-h f"   'counsel-describe-function
                "C-h v"   'counsel-describe-variable
                :autoload "iex-ivy")

(lib-key-define "C-h k" 'helpful-key :autoload "helpful")

(with-eval-after-load 'swiper
  (lib-key-define :keymap swiper-map [escape] 'minibuffer-keyboard-quit))

(with-eval-after-load 'ivy
  (lib-key-define :keymap ivy-minibuffer-map
                  "<C-return>" 'ivy-immediate-done
                  [escape] 'minibuffer-keyboard-quit))
(with-eval-after-load 'yasnippet
  (lib-key-define "C-c iy" 'ivy-yasnippet
                  :keymap yas-minor-mode-map
                  :autoload "ivy-yasnippet"))

;; iex-elfeed
(lib-key-define "C-z w" 'elfeed :autoload "iex-elfeed")
(with-eval-after-load 'elfeed
  (lib-key-define "?" 'toggle-hydra-elfeed/body
                  :keymap elfeed-search-mode-map
                  :autoload "iex-elfeed"))

;; iex-git
;; transient file
(setq transient-history-file
      (concat lye-emacs-cache-dir "transient/history.el"))
(setq transient-values-file
      (concat lye-emacs-cache-dir "transient/values.el"))
(setq transient-levels-file
      (concat lye-emacs-cache-dir "transient/levels.el"))

;; Forge configuration
(setq forge-database-file
      (expand-file-name "forge-database.sqlite" lye-emacs-cache-dir))
(lib-key-define "C-x g" 'one-key-magit/menu :autoload "iex-git")

;; iex-window
(lib-key-define "C-x 4 u" 'winner-undo
                "C-x 4 r" 'winner-redo
                :autoload "iex-window")

;; iex-avy
(lib-key-define "M-s" 'one-key-avy/menu :autoload "iex-avy")

;; iex-vterm
(lib-key-define "<f5>" 'shell-pop :autoload "iex-term")

;; iex-pomidor.el
(lib-key-define "C-z s c" 'pomidor :autoload "iex-pomidor")

;; open line in browser
;; see @https://github.com/noctuid/link-hint.el/
(lib-key-define "C-x p o" 'link-hint-open-link
                "C-x p c" 'link-hint-copy-link
                :autoload "link-hint")

(with-eval-after-load 'org
  (lib-key-define "C-x p i" 'org-cliplink
                  :keymap org-mode-map :autoload "org-cliplink"))

;; lex-snails
(when (and (not IS-WINDOWS) (display-graphic-p))
  (lib-key-define "C-x b" 'snails
                  "C-z C-s" 'snails-load-theme
                  :autoload "iex-snails"))
;; iex-tldr
(unless IS-WINDOWS
  (lib-key-define "C-z s h" 'tldr :autoload "iex-tldr"))

;; iex-smart-align
(lib-key-define "C-z s m" 'smart-align
                :autoload "smart-align"
                :keymap prog-mode-map)

;; iex-winum
(with-eval-after-load 'winum
  (lib-key-define :keymap winum-keymap
                  "M-0" 'winum-select-window-0-or-10
                  "M-1" 'winum-select-window-1
                  "M-2" 'winum-select-window-2
                  "M-3" 'winum-select-window-3
                  "M-4" 'winum-select-window-4
                  "M-5" 'winum-select-window-5
                  "M-6" 'winum-select-window-6
                  "M-7" 'winum-select-window-7
                  "M-8" 'winum-select-window-8
                  "M-9" 'winum-select-window-9))

;; iex-ace-window
(lib-key-define [remap other-window] 'ace-window)
(with-eval-after-load 'ace-window
  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (define-key (current-global-map)
        (kbd (format "M-%d" (1+ n)))
      (lambda () (interactive) (aw--select-window (1+ n)))))
  (with-eval-after-load 'super-save
    (advice-add 'aw--select-window :before #'super-save-command-advice)
    (advice-add 'ace-window :before #'super-save-command-advice)
    (push 'aw--select-window super-save-triggers)
    (push 'ace-window super-save-triggers)))

;; adjust-opacity
(lib-key-define :prefix "C-, u"
                "-" (lambda () (interactive) (lye//adjust-opacity nil -2))
                "=" (lambda () (interactive) (lye//adjust-opacity nil 2))
                "0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(lib-key-define "C-x C-s" #'super-save-all-buffer)

;; lsp
(with-eval-after-load 'lsp-ui
  (lib-key-define [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
                  [remap xref-find-references] 'lsp-ui-peek-find-references
                  "C-c u" 'lsp-ui-imenu
                  :keymap lsp-ui-mode-map))

(provide 'core-key)

;;; core-key.el ends here
