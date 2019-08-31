;;; iex-doom-modeline.el --- Initialize doom-modeline -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (doom-modeline)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: mode-line


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

;; doom-modeline

;;; Code:


(defun install-doom-modeline ()
  (package! 'doom-modeline t t)
  (package! 'all-the-icons nil t)
  (package! 'shrink-path nil t)
  (package! 'dash nil t))

;;;###autoload
(defun enable-doom-modeline ()
  "Enable `doom-modeline'"
  (interactive)
  (install-doom-modeline)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-mode +1))

;;;###autoload
(defun disable-doom-modeline ()
  (interactive)
  (doom-modeline-mode -1))

(provide 'iex-doom-modeline)

;;; iex-doom-modeline.el ends here
