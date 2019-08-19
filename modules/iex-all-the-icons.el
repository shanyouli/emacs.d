;;; iex-all-the-icons.el --- Initialize all-the-icons -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (all-the-icons)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: icons


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

;; Add some icons of files that are not collected by all-the-icons-icon-alist

;;; Code:
(unless (featurep 'package)
  (lye/core-require 'core-elpa))

(require-package 'all-the-icons)
(require 'all-the-icons)

(push '("\\.lua\\(\\.template\\|\\)$" all-the-icons-fileicon  "lua"
        :height 1.0 :face all-the-icons-purple)
      all-the-icons-icon-alist)

(push '("\\.mkv$" all-the-icons-faicon "film" :face all-the-icons-blue)
      all-the-icons-icon-alist)

(push '(vterm-mode all-the-icons-faicon "terminal" :face all-the-icons-yellow)
      all-the-icons-mode-icon-alist)

(provide 'iex-all-the-icons)

;;; iex-all-the-icons.el ends here
