;;; ui-all-the-icons.el --- Initialize All-The-Icons -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: icons
;; Last-Updated: 2019-11-29 16:28:12


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

;; Initalize all-the-icons

;;; Change log:
;;
;; 11/29/19

;;; Code:

(require 'all-the-icons)

(with-eval-after-load 'all-the-icons
  ;;
  ;;; icons
  (push '("\\(\\.\\|\\)conf\\(ig\\|\\)\\'" all-the-icons-fileicon "config"
          :height 1.0 :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.lua\\(\\.template\\|\\)\\'" all-the-icons-fileicon  "lua"
          :height 1.0 :face all-the-icons-blue)
        all-the-icons-icon-alist)

  (push '("\\.mkv\\'" all-the-icons-faicon "film" :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.rasi\\'" all-the-icons-alltheicon "css3" :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.ebuild\\'" all-the-icons-fileicon "gentoo" :face all-the-icons-purple)
        all-the-icons-icon-alist)

  ;;
  ;;; mode icons

  (push '(vterm-mode all-the-icons-faicon "terminal" :face all-the-icons-yellow)
        all-the-icons-mode-icon-alist)

  (push '(conf-mode all-the-icons-fileicon "config" :face all-the-icons-blue)
        all-the-icons-mode-icon-alist)

  (push '(conf-unix-mode all-the-icons-fileicon "config" :face all-the-icons-yellow)
        all-the-icons-mode-icon-alist)
  (push '(ebuild-mode all-the-icons-fileicon "gentoo" :face all-the-icons-purple)
        all-the-icons-mode-icon-alist)

  ;;
  ;;; dir-icons

  (push '("Videos?" all-the-icons-faicon "film") all-the-icons-dir-icon-alist)
  (push '("Work" all-the-icons-material "work") all-the-icons-dir-icon-alist))

(provide 'ui-all-the-icons)

;;; ui-all-the-icons.el ends here
