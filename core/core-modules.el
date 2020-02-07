;;; core-modules.el --- Initialize modules -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: modules
;; Last-Updated: 2019-11-20 11:06:33


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

;; `lye-modules-dir'初始化

;;; Change log:
;;
;; 11/20/19

;;; Code:

(defcustom lye-modules-package-list '()
  "`lye-modules-dir' 中需要的 packages"
  :type 'list)

(setq lye-modules-package-list
      '(
        ;; iex-fuz
        fuz
        ;; ivy-fuz
        flx
        ivy-yasnippet
        ;; lex-ido
        ido-completing-read+
        smex
        flx-ido
        ido-sort-mtime
        ;; md-hugo
        ox-hugo
        easy-hugo
        ;;
        cnfonts
        ;; iex-awetab
        (awesome-tab :type git
                     :host github
                     :repo "manateelazycat/awesome-tab"
                     :no-byte-compile t)
        ;; iex-awetray
        (awesome-tray :type git
                      :host github
                      :repo "manateelazycat/awesome-tray"
                      :no-byte-compile t)
        ;; iex-doom-modeline
        doom-modeline
        ;; md-org
        org-bullets
        ob-go
        ob-rust
        ob-ipython
        htmlize
        org-cliplink))
(mapc #'package+ lye-modules-package-list)

(package+ 'pdf-tools)

;;
;;;
(package+ 'nov)

(package+ 'cmuscheme)
(package+ 'paredit)

(provide 'core-modules)

;;; core-modules.el ends here
