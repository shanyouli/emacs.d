;;; iex-git.el --- Init for git -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (magit magithub)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: git


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

;; Init for git

;;; Code:


(require 'magit)






(when (locate-library "diff-hl")
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'iex-git)

;;; iex-git.el ends here
