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
(package! '(snails :type git
                   :host github
                   :repo "manateelazycat/snails"
                   :no-byte-compile t)
          t)

(package! '(snails-backend-themes :type git
                                  :host github
                                  :repo "shanyouli/snails-backend"
                                  :no-bype-compile t)
          t)

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
              "*esup"))
  (push buf snails-backend-buffer-blacklist))

(provide 'iex-snails)

;;; iex-snails.el ends here
