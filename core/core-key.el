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

(lib-key-unset "C-z" "C-SPC" "C-\\" "C-x s" "C-r" "C-x C-SPC")

;; esup
(lib-key-define :autoload "esup" "C-, s e" #'esup)
(lib-key-define "C-, s b" #'core-benchmark/require-times)
;; restart-emacs
(lib-key-define "C-, s r" #'restart-emacs
                :autoload "restart-emacs")

;; Chinese automatically translated as English
(lib-key-define :autoload "insert-translated-name"
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


(lib-key-define "C-c y" 'lye/dict-point)

;; lex-search.el
(lib-key-define "C-c s" 'one-key-color-rg-search/menu :autoload "lex-search")

;; lex-thing-edit.el


;; lex-pyim.el
(lib-key-define "<f9>" 'toggle-input-method
                "C-<f9>" 'lye/toggle-pyim-punctuation-translate)

;; lex-funcs
(lib-key-define "C-z f" 'hydra-functions-menu/body :autoload "lex-funcs")

;; md-tmp-ext
(lib-key-define "C-, ot" 'one-key-tmp-scratch/menu :autoload "md-tmp-ext")

;; awesome-tab.el
(lib-key-define "C-z j" 'awesome-tab-ace-jump :autoload "awesome-tab")

;; lex-smex
;; (lib-key-define "M-x"     'smex
;;                 "C-x M-x" 'smex-major-mode-commands
;;                 :autoload "lex-ido")

;;; toolkit
(lib-key-define "C-x SPC" 'set-mark-command    ; Instead C-SPC for Chinese input method
                "C-x C-h" 'rectangle-mark-mode ; rectangle-mark-mode
                "C-z C-z" 'suspend-frame)    ; Suspend-frame
;; iv-bundle
(lib-key-define ;; swiper
                "C-s" 'swiper-isearch
                "C-r" 'swiper-isearch-backward
                "s-f" 'swiper
                "C-S-s" 'swiper-all
                ;; ivy
                "C-c C-r" 'ivy-resume
                "C-c v p" 'ivy-push-view
                "C-c v ." 'ivy-switch-view
                ;; counsel
                "C-x C-r" 'counsel-buffer-or-recentf
                "C-x j" 'counsel-mark-ring
                "C-h F" 'counsel-faces
                "C-c B" 'counsel-bookmarked-directory
                "C-c O" 'counsel-find-file-extern
                "C-c f" 'counsel-find-library
                "C-c i" 'counsel-git
                "C-c j" 'counsel-git-grep
                "C-c l" 'counsel-locate
                "C-c r" 'counsel-rg
                "C-c z" 'counsel-fzf
                "C-c c B" 'counsel-bookmarked-directory
                "C-c c F" 'counsel-faces
                "C-c c F" 'counsel-faces
                "C-c c L" 'counsel-load-library
                "C-c c O" 'counsel-find-file-extern
                "C-c c P" 'counsel-package
                "C-c c a" 'counsel-apropos
                "C-c c e" 'counsel-colors-emacs
                "C-c c f" 'counsel-find-library
                "C-c c g" 'counsel-grep
                "C-c c h" 'counsel-command-history
                "C-c c i" 'counsel-git
                "C-c c j" 'counsel-git-grep
                "C-c c l" 'counsel-locate
                "C-c c m" 'counsel-minibuffer-history
                "C-c c o" 'counsel-outline
                "C-c c p" 'counsel-pt
                "C-c c r" 'counsel-rg
                "C-c c s" 'counsel-ag
                "C-c c t" 'counsel-load-theme
                "C-c c u" 'counsel-unicode-char
                "C-c c w" 'counsel-colors-web
                "C-c c v" 'counsel-set-variable
                "C-c c z" 'counsel-fzf)

(with-eval-after-load 'counsel
  (lib-key-define :keymap counsel-mode-map
                  [remap swiper]          'counsel-grep-or-swiper
                  [remap swiper-backward] 'counsel-gre-or-swiper-backward
                  [remap dired]           'counsel-dired
                  [remap set-variable]    'counsel-set-variable
                  [remap insert-char]     'counsel-unicode-char)
  (lib-key-define :map counsel-find-file-map
                  "C-h" 'counsel-up-directory)
  (lib-key-define :map counsel-ag-map
                  "<C-return>" 'my-swiper-toggle-counsel-rg))

(with-eval-after-load 'swiper
  (lib-key-define :keymap swiper-map
                  [escape] 'minibuffer-keyboard-quit
                  "M-s" 'swiper-isearch-toggle
                  "M-%" 'swiper-query-replace))

(with-eval-after-load 'ivy
  (lib-key-define :keymap ivy-minibuffer-map
                  "<C-return>" 'ivy-immediate-done
                  [escape] 'minibuffer-keyboard-quit
                  "C-w" 'ivy-yank-word))

(with-eval-after-load 'yasnippet
  (lib-key-define "C-c i y" 'ivy-yasnippet
                  :keymap yas-minor-mode-map
                  :autoload "ivy-yasnippet"))

;; Rss-bundles
(lib-key-define "C-x W" 'newsticker-show-news
                "C-x w" 'elfeed)
(with-eval-after-load 'elfeed
  (lib-key-define "?" 'elfeed-hydra/body
                  :keymap elfeed-search-mode-map)
  (lib-key-define :map elfeed-show-mode-map "q" 'delete-window)
  (cond
    ((fboundp 'link-hint-open-link)
     (lib-key-define :map elfeed-show-mode-map "o" 'link-hint-open-link))
    ((fboundp 'ace-link)
     (lib-key-define :map elfeed-show-mode-map "o" 'ace-link))))

;; company-bundle
(lib-key-define "M-/" 'company-complete
                "<backtab>" 'company-yasnippet
                "C-c t" 'company-backend-with-tabnine
                "C-c T" 'company-backend-remove-tabnine)

(with-eval-after-load 'company
  (lib-key-define :map company-active-map
                  "C-p" 'company-select-previous
                  "C-n" 'company-select-next
                  "<tab>" 'company-complete-common-or-cycle
                  "<backtab>" 'lye-company-yasnippet)
  (lib-key-define "C-p" 'company-select-previous
                  "C-n" 'company-select-next
                  :map company-search-map)
  (dotimes (i 10)
    (lib-key-unset company-active-map (format "M-%d" i))))

;; dired-bundle
(with-eval-after-load 'dired
  (lib-key-define :map dired-mode-map
                  "C-c C-p" 'wdired-change-to-wdired-mode
                  ")" 'dired-git-info-mode
                  "C-c C-r" 'dired-rsync
                  "H" 'dired-omit-mode)
  (when (and (or (and IS-MAC (executable-find "gls"))
                 (and (not IS-MAC) (executable-find "ls")))
             (bundle-active-p 'hydra))
    (lib-key-define :map dired-mode-map "S" 'hydra-dired-quick-sort/body)))

;; mode-bundle
(when (executable-find "markdownfmt")
  (with-eval-after-load 'markdown-mode
    (lib-key-define :map markdown-mode-map
                    "C-c f" 'markdownfmt-format-buffer)))

(lib-key-define "C-z s m" 'smart-align
                :keymap prog-mode-map)

;; editor-bundle
(lib-key-define "M-e"  'one-key-thing-edit/menu)

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


;; iex-avy
(lib-key-define "M-s" 'one-key-avy/menu :autoload "iex-avy")

;; iex-vterm
(lib-key-define "<f5>" 'shell-pop)

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
                  "C-z C-s" 'snails-load-theme)
  (with-eval-after-load 'snails
    (lib-key-define :map snails-mode-map
                    "<up>" 'snails-select-prev-item
                    "<down>" 'snails-select-next-item
                    "<left>" 'snails-select-prev-backend
                    "<right>" 'snails-select-next-backend))
  )

;; iex-tldr
(unless IS-WINDOWS
  (lib-key-define "C-z s h" 'tldr :autoload "iex-tldr"))



;; window-bundle
(lib-key-define "C-h z" 'shackle-last-popup-buffer)
(pcase lye-use-switch-windows-package
  ('ace-window
   (lib-key-define [remap other-window] 'ace-window
                   "M-0" 'aw-select-window-0
                   "M-1" 'aw-select-window-1
                   "M-2" 'aw-select-window-2
                   "M-3" 'aw-select-window-3
                   "M-4" 'aw-select-window-4
                   "M-5" 'aw-select-window-5
                   "M-6" 'aw-select-window-6
                   "M-7" 'aw-select-window-7
                   "M-8" 'aw-select-window-8
                   "M-9" 'aw-select-window-9))
  ('winum
   (lib-key-define "M-0" 'winum-select-window-0-or-10
                   "M-1" 'winum-select-window-1
                   "M-2" 'winum-select-window-2
                   "M-3" 'winum-select-window-3
                   "M-4" 'winum-select-window-4
                   "M-5" 'winum-select-window-5
                   "M-6" 'winum-select-window-6
                   "M-7" 'winum-select-window-7
                   "M-8" 'winum-select-window-8
                   "M-9" 'winum-select-window-9)))

(lib-key-define "C-x 4 u" 'winner-undo
                "C-x 4 r" 'winner-redo)

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

;; elisp-bundles
(lib-key-define :keymap emacs-lisp-mode-map
                :prefix "C-c"
                "C-x" 'ielm
                "C-c" 'eval-defun
                "C-b" 'eval-buffer
                "e" 'macrostep-expand)
(lib-key-define :keymap help-mode-map "r" 'remove-hook-at-point)
(lib-key-define [remap describe-key] 'helpful-key
                [remap describe-symbol] 'helpful-symbol)
(with-eval-after-load 'helpful
  (lib-key-define :keymap helpful-mode-map "r" 'remove-hook-at-point))

(provide 'core-key)

;;; core-key.el ends here
