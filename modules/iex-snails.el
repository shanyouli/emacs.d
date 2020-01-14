;;; iex-snails.el --- Multiple backend search tools -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.3
;; Package-Requires: (snails fuz)
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

;; snails

;;; require

(require 'snails)
(require 'snails-backend-themes)

;;; Code:

;;; Use fuzzy search from `fuz'
(lye/modules-require 'iex-fuz)
(setq snails-fuz-library-load-status "load")
;; -----------------------------------------------------------------------------

;;; snails-backend-buffer-blacklist
(dolist (buf (list
              " *which-key*"
              " *straight-process*"
              "*straight-process*"
              "*One-Key*"
              "*Flycheck**"
              "*flycheck-posframe-buffer*"
              " *flycheck-posframe-buffer*"
              " *company-posframe-buffer*"
              "*company"
              " *company"
              "*esup"
              " *pyim"
              " *server"
              " *sdcv"
              " *diff-hl*"))
  (push buf snails-backend-buffer-blacklist))

;;; snails-backend-themes
(defun snails-load-theme ()
  "Loading a theme use `snails'"
  (interactive)
  (snails '(snails-backend-themes)))

(add-hook 'snails-mode-hook
          (lambda ()
            (if (and (featurep 'evil) evil-mode)
                (evil-emacs-state))))

(add-hook 'snails-mode-hook
          ;; UP, Down, Left, Right key bind
          (defun +arrow-key-and-snails ()
            (define-key snails-mode-map (kbd "<up>") 'snails-select-prev-item)
            (define-key snails-mode-map (kbd "<down>") 'snails-select-next-item)
            (define-key snails-mode-map (kbd "<left>") 'snails-select-prev-backend)
            (define-key snails-mode-map (kbd "<right>") 'snails-select-next-backend)))

(provide 'iex-snails)

;;; iex-snails.el ends here
