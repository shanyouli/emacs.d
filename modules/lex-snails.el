;;; lex-snails.el --- Multiple backend search tools -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (snails)
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

;;; Code:

(require 'fuz)
(require 'snails)
(setq snails-fuz-library-load-status "load")

;; Make the theme of the snails window change as the theme of the change changes
(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'snails-init-face-with-theme))

(provide 'lex-snails)

;;; lex-snails.el ends here
