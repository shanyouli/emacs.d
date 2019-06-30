;;; init-edit.el ---Initialize Edit Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye liivy-alt-done <shanyouli6@gamil.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
;; Optional
;; (setq locale-coding-system 'utf-8)
;; (unless system/windows
;; (setq selection-coding-system 'utf-8))

;; Miscs
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if name are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t) ; Deleting file go to OS'trash floder
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after poping mark pops it again

(setq-default major-mode 'text-mode)
;; (add-hook 'text-mode-hook (lambda ()
;;                             (turn-on-auto-fill)
;;                             (diminish 'auto-fill-function)))
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Do not use the mouse in the graphical interface
(when (display-graphic-p)
  (use-package disable-mouse :hook  (after-init . global-disable-mouse-mode)))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-worf-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background 1))

;; Pair Automatic completion
(use-package autopair
  :diminish autopair-mode
  :hook (after-init . autopair-global-mode))
(use-package rainbow-delimiters
  :hook (autopair-mode . rainbow-delimiters-mode))

;;Chinese input automatically adds spaces in Chinese
;;(use-package pangu-spacing
;;  :diminish pangu-spacing-mode
;;  :hook (after-init . global-pangu-spacing-mode))

;;Big blank delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode))

;; Show native line numbers if possible, otherwise use linum
(if (and (display-graphic-p) (< lye-frame-default-width 86))
    (global-display-line-numbers-mode -1)
  (if (fboundp 'display-line-numbers-mode)
      (use-package display-line-numbers
        :ensure nil
        :hook (prog-mode . display-line-numbers-mode)
        :config
        (setq line-number-display-limit large-file-warning-threshold)
        (setq line-number-display-limit-width 1000))
    (use-package linum-off
      :demand
      :defines linum-format
      :hook (after-init . global-linum-mode)
      :config
      (setq linum-format "%4d "))))

;; Don't display `symbolic link to Git-controlled source file....'
;; @see https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks nil)

;;Set blank highlight when use display graphic
(if  (display-graphic-p)
    (use-package highlight-indent-guides
      :hook (prog-mode . highlight-indent-guides-mode)
      :config
      (setq highlight-indent-guides-method 'character)
      (setq highlight-indent-guides-responsive t)))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

;; Automatically refresh files that have been changed elsewhere
(add-hook 'after-init-hook (lambda () (global-auto-revert-mode t)))

;; add color display
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))

;;; Uninstall some global shortcuts that may cause conflicts
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-r"))
;; (global-unset-key [^s])

;; isearch or swiper
(if (locate-library "swiper")
    (use-package isearch
      :ensure nil
      :bind (("C-s s" . isearch-forward)
             ("C-s r" . isearch-backward)
             :map isearch-mode-map
             ("C-n" . isearch-repeat-forward)
             ("C-p" . isearch-repeat-backward))))

;;; Highlight symbols
;; Usage:
;; When press M-s active lazy-search, it will mark current symbol or region.
;; You can press below keys to jump in all matching symbols,
;; or press `q' to quitlazy-search mode.
(use-package lazy-search
  :ensure nil
  :commands (lazy-search)
  :bind ("C-s l" . lazy-search)
  :init
  (advice-add #'lazy-search :after
            #'(lambda () (rainbow-mode -1)))
  (advice-add #'lazy-search-quit :after
              #'(lambda () (rainbow-mode t))))

;; open line in browser
;; at see@https://github.com/noctuid/link-hint.el/
(use-package link-hint
  :bind
  ("C-x p o" . link-hint-open-link)
  ("C-x p c" .  link-hint-copy-link))

;; Search and refactoring tool based on ripgrep
;; see @https://github.com/manateelazycat/color-rg
(when  (executable-find "rg")
  (use-package color-rg
    :ensure nil
;;    :preface (unbind-key "C-s")
    :commands (color-rg-search-input
               color-rg-search-symbol
               color-rg-search-input-in-project
               color-rg-search-symbol-in-project
               color-rg-search-input-in-current-file
               color-rg-search-symbol-in-current-file)
    :bind (("C-s g" . color-rg-search-symbol)
           ("C-s h" . color-rg-search-input)
           ("C-s j" . color-rg-search-symbol-in-project)
           ("C-s k" . color-rg-search-input-in-project)
           ("C-s ," . color-rg-search-symbol-in-current-file)
           ("C-s ." . color-rg-search-input-in-current-file))))

;; Quickly search the copy history and paste it at the cursor
;; @https://emacs-china.org/t/c-k/6775/9
(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :bind ("C-s p" . browse-kill-ring))

(provide 'init-edit)
;;; init-edit.el ends here
