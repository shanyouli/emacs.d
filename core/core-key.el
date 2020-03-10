;;; core/core-key.el.el -*- lexical-binding: t -*-

;; Mainly the shortcut keys for local and third-party packages

;; Global uninstall button
(require 'lib-key)

(lib-key-unset "C-z" "C-SPC" "C-\\" "C-x s" "C-x C-SPC" "M-," "M-.")
(defconst syl-key "M-,")
(lib-key-definer syl-key)
(syl-key "xs" 'super-save-all-buffer
         ;; esup
         "se" #'esup
         ;; benchmark
         "sb" #'core-benchmark/require-times
         ("sr" 'restart-emacs (require 'restart-emacs nil t))
         ;; hydra-bundle
         "ub" 'hydra-ui-menu/body
         "od" 'hydra-open-dir-menu/body
         "f"  'one-key-functions/menu
         "ot" 'one-key-change-fontsize/menu
         "ua" 'one-key-adjust-opacity/menu
         )

;; dict-bundle
(lib-key "C-c y" 'lye/dict-point)

;; pyim-bundle
(lib-keys "<f9>" 'lye/toggle-input-method
          "C-<f9>" 'bundle-pyim-punctuation-toggle)

;;; toolkit
(lib-keys "C-x SPC" 'set-mark-command    ; Instead C-SPC for Chinese input method
          "C-x C-h" 'rectangle-mark-mode ; rectangle-mark-mode
          "C-z C-z" 'suspend-frame)    ; Suspend-frame
;; ivy-bundle
(lib-keys "C-s" 'swiper-isearch
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
(lib-keys :map counsel-mode-map
          :package counsel
          [remap swiper]          'counsel-grep-or-swiper
          [remap swiper-backward] 'counsel-gre-or-swiper-backward
          [remap dired]           'counsel-dired
          [remap set-variable]    'counsel-set-variable
          [remap insert-char]     'counsel-unicode-char)
(lib-keys :map counsel-find-file-map
          :package counsel
          "C-h" 'counsel-up-directory)
(lib-keys :map counsel-ag-map
          :package counsel
          "<C-return>" 'my-swiper-toggle-counsel-rg)
(lib-keys :map swiper-map
          :package swiper
  [escape] 'minibuffer-keyboard-quit
  "M-s" 'swiper-isearch-toggle
  "M-%" 'swiper-query-replace)
(lib-keys :map ivy-minibuffer-map
          :package ivy
  "<C-return>" 'ivy-immediate-done
  [escape] 'minibuffer-keyboard-quit
  "C-w" 'ivy-yank-word)
(with-eval-after-load 'yasnippet
  (lib-key "C-c i y" 'ivy-yasnippet
           yas-minor-mode-map
           (require 'ivy-yasnippet nil t)))
;;  or snails-bundle
(lib-keys "C-x b" 'snails
          "C-c c t" 'snails-load-theme
          :filter (and (not IS-WINDOWS) (display-graphic-p)))
(lib-keys :map snails-mode-map
          :package snails
  "<up>" 'snails-select-prev-item
  "<down>" 'snails-select-next-item
  "<left>" 'snails-select-prev-backend
  "<right>" 'snails-select-next-backend)

;; Rss-bundles
(lib-keys "C-x W" 'newsticker-show-news
          "C-x w" 'elfeed)
(lib-keys "?" 'elfeed-hydra/body
          :map elfeed-search-mode-map
          :package 'elfeed)
(lib-keys "q" 'delete-window
          "o" (lambda ()
                (interactive)
                (cond
                 ((fboundp 'link-hint-open-link)
                  (call-interactively 'link-hint-open-link))
                 ((fboundp 'ace-link)
                  (call-interactively 'ace-link))))
          :map elfeed-show-mode-map
          :package 'elfeed)


;; company-bundle
(lib-keys "M-/" 'company-complete
          "<backtab>" 'company-yasnippet
          "C-c t" 'company-backend-with-tabnine
          "C-c T" 'company-backend-remove-tabnine
          "C-, i t" 'toggle-company-english-helper)
;; English word completion with Chinese comments
(with-eval-after-load 'company
  (lib-keys :map company-active-map
    "C-p" 'company-select-previous
    "C-n" 'company-select-next
    "<tab>" 'company-complete-common-or-cycle
    "<backtab>" 'lye-company-yasnippet)
  (lib-keys "C-p" 'company-select-previous
            "C-n" 'company-select-next
            :map company-search-map)
  (dotimes (i 10)
    (lib-key-unset company-active-map (format "M-%d" i))))

;; dired-bundle
(lib-keys :map dired-mode-map
          :package dired
  "C-c C-p" 'wdired-change-to-wdired-mode
  ")" 'dired-git-info-mode
  "C-c C-r" 'dired-rsync
  "H" 'dired-omit-mode
  ("S"
   'hydra-dired-quick-sort/body
   (and (or (and IS-MAC (executable-find "gls"))
            (and (not IS-MAC) (executable-find "ls")))
        (bundle-active-p 'hydra))))

;; mode-bundle
(lib-keys :map markdown-mode-map
          :package markdown-mode
          :filter (executable-find "markdownfmt")
  "C-c f" 'markdownfmt-format-buffer)

(lib-keys "C-z s m" 'smart-align
          :map prog-mode-map)

;; editor-bundle
(lib-keys "M-e"  'one-key-thing-edit/menu
          "C-:" 'avy-goto-char
          "C-'" 'avy-goto-char-2
          "M-g f" 'avy-goto-line
          "M-g w" 'avy-goto-word-1
          "M-g e" 'avy-goto-word-0)
;; Chinese automatically translated as English
(syl-key
  "io" 'insert-translated-name-insert-original-translation
  "iu" 'insert-translated-name-insert-with-underline
  "il" 'insert-translated-name-insert-with-line
  "ic" 'insert-translated-name-insert-with-camel)
(lib-key "C-c s" 'one-key-color-rg-search/menu nil (fboundp 'color-rg-search-symbol))
(lib-key "C-z s b" 'backup-file-log nil (fboundp 'backup-file))
;; treemacs-bundle
(lib-keys [f8] 'treemacs
          "M-0" 'treemacs-select-window
          "C-x t t" 'treemacs
          "C-x t b" 'treemacs-bookmark
          "C-x t M-t" 'treemacs-find-tag)
(with-eval-after-load 'treemacs
  (lib-keys "C-x 1" 'treemacs-delete-other-windows
            "C-x t 1" 'treemacs-delete-other-windows)
  (lib-keys :map projectile-command-map
            :package projectile
    "h" 'treemacs-projectile))

;; Git-bundle
(lib-keys "C-x g" 'one-key-magit/menu
          "C-x M-g" 'magit-dispatch
          "C-c M-g" 'magit-file-popup)

;; unset C-x g
;; @see https://github.com/magit/magit/issues/3522#issuecomment-407640436
(with-eval-after-load "magit-files"
  ;; (define-key magit-file-mode-map (kbd "C-x g") nil)
  (lib-key-unset magit-file-mode-map "C-x g"))
(with-eval-after-load "magit-mode"
  (lib-key-unset magit-mode-map "M-1" "M-2" "M-3"))

;; term-bundle
(lib-key "<f5>" 'shell-pop)
(syl-key "pi" 'org-cliplink
         :autoload "org-cliplink"
         :map org-mode-map
         :package org)

;; window-bundle
(lib-key "C-h z" 'shackle-last-popup-buffer)
(pcase lye-use-switch-windows-package
  ('ace-window
   (lib-keys [remap other-window] 'ace-window
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
   (lib-keys "M-0" 'winum-select-window-0-or-10
             "M-1" 'winum-select-window-1
             "M-2" 'winum-select-window-2
             "M-3" 'winum-select-window-3
             "M-4" 'winum-select-window-4
             "M-5" 'winum-select-window-5
             "M-6" 'winum-select-window-6
             "M-7" 'winum-select-window-7
             "M-8" 'winum-select-window-8
             "M-9" 'winum-select-window-9)))

(lib-keys "C-x 4 u" 'winner-undo
          "C-x 4 r" 'winner-redo)

;; lsp
(lib-keys [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
          [remap xref-find-references] 'lsp-ui-peek-find-references
          "C-c u" 'lsp-ui-imenu
          :map lsp-ui-mode-map
          :package lsp-ui)

;; elisp-bundles
(lib-keys :map emacs-lisp-mode-map
          :prefix "C-c"
  "C-x" 'ielm
  "C-c" 'eval-defun
  "C-b" 'eval-buffer
  "e" 'macrostep-expand)
(lib-keys :map help-mode-map "r" 'remove-hook-at-point)
(lib-keys [remap describe-key] 'helpful-key
          [remap describe-symbol] 'helpful-symbol)
(with-eval-after-load 'helpful
  (lib-key "r" 'remove-hook-at-point helpful-mode-map))

;; tools-bundle
(syl-key "po" 'link-hint-open-link
         "pc" 'link-hint-copy-link
         "tp" 'pomidor
         ("sh" 'tldr (not IS-WINDOWS))
         ("uc" 'cnfonts-ui (fboundp 'cnfonts-ui)))
