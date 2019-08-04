;;; iex-flycheck.el --- Initialize flycheck -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (flycheck flycke-posframe)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: flychek


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

;; commentary

;;; Code:

(require-package 'flycheck)
(require 'flycheck)

;; flycheck-configurations
(setq flycheck-indication-mode 'right-fringe)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Only check while saving and opening files
(setq flycheck-check-syntax-automatically '(save mode-enabled))

(when (display-graphic-p)
  (require-package 'flycheck-posframe)
  (require 'flycheck-posframe)
  (add-hook 'after-init-hook 'flycheck-posframe-mode))

(flycheck-mode t)

(provide 'iex-flycheck)

;;; iex-flycheck.el ends here