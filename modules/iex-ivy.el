;;; iex-ivy.el --- Initialize IVY configurations. -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (ivy)
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

;; IVY SWIPER, counsel

;;; Code:
(require-package 'ivy)
(require-package 'counsel)
(require 'counsel)

(pcase lye-use-amx-or-fuzzy
  ('fuzzy
   (if (and (locate-library "fuz-core") (locate-library "ivy-fuz"))
       (lye/modules-require 'lex-fuz)))
  ('amx
   (if (locate-library "amx")
       (lye/modules-require 'iex-amx))))

(setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)      ; Enable bookmarks and recentf
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function nil)
(setq ivy-display-style 'fancy)

;; @https://github.com/abo-abo/swiper/issues/2130
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)

;; Fuzzy search
;; (if (locate-library "flx")
;;     (setq ivy-re-builders-alist
;;           '((t . ivy--regex-fuzzy))))

(when (locate-library "magit")
  (setq magit-completing-read-function 'ivy-completing-read))

;; Use faster search tools: rigprep
(let ((command
       (cond
        ((executable-find "rg")
         "rg -i M 120 --no-beading --line-number --color never '%s' %s"))))
  (setq counsel-grep-base-command command))

;; swiper
(setq swiper-action-recenter t)

;; helpful
(when (locate-library "helpful")
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

;; load-locate-key
(lazy-load-set-keys '(([escape] . minibuffer-keyboard-quit)) swiper-map)

(lazy-load-set-keys
 '(("<C-return>" . ivy-immediate-done)
   ([escape]     . minibuffer-keyboard-quit))
 ivy-minibuffer-map)

(ivy-mode 1)
(counsel-mode 1)

(provide 'iex-ivy)

;;; iex-ivy.el ends here
