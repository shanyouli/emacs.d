;;; iex-paradox.el --- Initialize Paradox -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (paradox)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: package tools


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

;; Package management tools

;;; Code:



(require-package 'paradox)
(require 'paradox)
(defun lye/list-package ()
  (interactive)
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)
  (paradox-enable)
  (list-packages))

(provide 'iex-paradox)

;;; iex-paradox.el ends here
