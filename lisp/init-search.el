;;; init-search.el --- Initialize Search Configurations -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (use-package lazy-search color-rg browse-kill-ring .etc)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: search


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

;; Initialize Search

;;; Code:

;;; Configurations

;; Uninstall some global shortcuts that may cause conflicts
(global-unset-key (kbd "C-s")) ; (global-unset-key [^s])
(global-unset-key (kbd "C-r"))

;; Isearch and swiper
(if (locate-library "swiper")
    (use-package swiper
      :ensure nil
      :bind (("C-s s" . swiper-isearch)
             :map swiper-map
             ([escape] . minibuffer-keyboard-quit))
      :config
      (setq swiper-action-recenter t))
  (use-package isearch
    :ensure nil
    :bind (("C-s s" . isearch-forward)
           ("C-s r" . isearch-backward))))

;; Highlight symbols
;; Usage:
;; When press `C-s-l' active lazy-search, it will mark current symbol or region.
;; You can press below keys to jump in all matching symbols,
;; or press `q' to quitlazy-search mode.
(use-package lazy-search
  :ensure nil
  :commands (lazy-search)
  :bind ("C-s l" . lazy-search)
  :custom-face
  (lazy-search-highlight-current
   ((t :foreground "black" :background "orange" :bold t)))
  (lazy-search-highlight-background
   ((t :forground "grey80" :background "green" :bold t)))
  :config
  (advice-add #'lazy-search :after
              #'(lambda () (rainbow-mode -1)))
  (advice-add #'lazy-search-quit :after
              #'(lambda () (rainbow-mode t))))

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


(provide 'init-search)

;;; init-search.el ends here
