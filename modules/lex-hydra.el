;;; lex-hydra.el --- one-keywords -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (one-key )
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

;; Quickly open some common folders and set shortcuts for some functions

;;; Code:

;;; The hot key about tool-bar, menu-bar, scroll-bar
(defhydra hydra-ui-menu (:exit nil)
  "GUI-Menu"
  ("t" tool-bar-mode "Tool-Bar")
  ("m" menu-bar-mode "Menu-Bar")
  ("s" scroll-bar-mode "Scroll-Bar")
  ("q" nil "quit"))

;;; Common file shortcuts
(defhydra hydra-open-dir-menu (:exit t)
  "Open the hot folder."
  ("d" (lambda ()
         (interactive)
         (dired-x-find-file (expand-file-name "~/.dotfiles"))) "Dotfiles")
  ("e" (lambda ()
         (interactive)
         (dired-x-find-file user-emacs-directory)) "Emacs.d")
  ("g" (lambda ()
         (interactive)
         (dired-x-find-file (expand-file-name "~/Git"))) "Git Repo")
  ("r" (lambda ()
         (interacitve)
         (dired-x-find-file (expand-file-name "~/Dropbox"))) "Dropbox")
  ("q" nil "quit"))

(provide 'lex-hydra)

;;; lex-hydra.el ends here
