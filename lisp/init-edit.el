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
(prefer-coding-system 'utf-8)

;; Miscs
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if name are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t) ; Deleting file go to OS'trash floder
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after poping mark pops it again

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook
	  (lambda ()
	    (turn-on-auto-fill)
	    (diminish 'auto-fill-function)))
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil)

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

;;Brackets highlighted
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (after-init . (lambda ()
			(global-highlight-parentheses-mode))))

;; Pair Automatic completion
(use-package autopair
  :diminish autopair-mode
  :hook (after-init . autopair-global-mode))

;;Chinese input automatically adds spaces in Chinese
;;(use-package pangu-spacing
;;  :diminish pangu-spacing-mode
;;  :hook (after-init . global-pangu-spacing-mode))

;;Big blank delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode))

;; Displays line-number.el
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-width 2)
  (setq display-line-numbers-grow-only t)
  ;; (set-face-foreground 'line-number-current-line "#859393")
  ;; (set-face-background 'line-number "#313335")
  (use-package linum-relative
    :commands linum-relative-mode
    :init
    (setq linum-relative-backend 'display-line-numbers-mode)
    (dolist (hook (list
	               'c-mode-common-hook
	               'emacs-lisp-mode-hook
	               'sh-mode-hook
	               'org-mode-hook))
      (add-hook hook (lambda () (display-line-numbers-mode)))
      ;;(add-hook hook (lambda () (linum-relative-mode 1)))
      )))

;; Don't display `symbolic link to Git-controlled source file....'
;; @see https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks nil)

;; Some major-mode are used manateelazycat/vi-navigate
(use-package vi-navigate
  :straight (vi-navigate :type git
                         :host github
                         :repo "manateelazycat/vi-navigate")
  :ensure nil
  :commands (vi-navigate-load-keys)
  :hook (after-init . vi-navigate-load-keys))

;;Set blank highlight when use display graphic
(if  (display-graphic-p)
    (use-package highlight-indent-guides
      :hook (prog-mode . highlight-indent-guides-mode)
      :config
      (setq highlight-indent-guides-method 'character)
      (setq highlight-indent-guides-responsive t))
  (use-package indent-guide
    :hook (prog-mode . indent-guide-mode)
    :config
    (set-face-background 'indent-guide-face "dimgray")
    (setq indent-guide-delay 0.1)
    (setq indent-guide-recursive t)
    (setq indent-guide-char "|")))


;; 80 wrap or set
;; see @https://stackoverflow.com/questions/18855510/have-emacs-highlight-characters-over-80
(defun lye/80-column ()
  "80-column?"
  (interactive)
  (require 'whitespace)
  (setq whitespace-line-column 80)
  (setq whitespace-action '(face lines-tail))
  (whitespace-mode))

;; Automatically refresh files that have been changed elsewhere
(add-hook 'after-init-hook (lambda () (global-auto-revert-mode t)))

(provide 'init-edit)
;;; init-edit.el ends here
