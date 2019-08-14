;;; lex-vi-key.el --- vim keybindings -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (hydra)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: key


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

;; vim key

;;; Code:

;; vi keybindings
(lye/modules-require 'lex-translate)
(defhydra hydra-vim-menu (:exit nil)
  "vim keybindings"
  ("h" backward-char "Left")
  ("j" next-line "Down Line")
  ("k" previous-line "Up Line")
  ("l" forward-char "Right")
  ("t" default-translate-point+ "Translate")
  ("q" nil))

(provide 'lex-vi-key)

;;; lex-vi-key.el ends here
