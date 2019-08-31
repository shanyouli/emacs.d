;;; iex-fuz.el --- fuzzy-match -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (fuz)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: fuzzy match


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

;; Fuzzy Match

;;; Code:

(package! '(fuz :host github :repo "cireu/fuz.el"
                :files ("fuz*.el" "src" "Cargo*")) t)

(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

(provide 'iex-fuz)

;;; iex-fuz.el ends here
