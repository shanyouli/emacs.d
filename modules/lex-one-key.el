;;; lex-one-key.el --- one-keywords -*- lexical-binding: t -*-

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
(defvar one-key-menu-ui-alist nil
  "The `one-key' menu alist for UI.")

(setq one-key-menu-ui-alist
      '((("t" . "Tool-Bar") . tool-bar-mode)
        (("m" . "Menu-Bar") . menu-bar-mode)
        (("s" . "Scroll-Bar") . scroll-bar-mode)))

(defun one-key-menu-ui ()
  "The `one-key' menu for UI."
  (interactive)
  (one-key-menu "UI" one-key-menu-ui-alist t))


;;; Common file shortcuts
(defvar one-key-menu-dir-alist nil
  "The `one-key' menu alist for dir.")

(setq one-key-menu-dir-alist
      '((("d" . "Dotfiles") . (lambda () (interactive) (dired-x-find-file "~/.dotfiles")))
        (("e" . "Emacs.d")  . (lambda () (interactive) (dired-x-find-file user-emacs-directory)))
        (("g" . "Git Repo") . (lambda () (interactive) (dired-x-find-file "~/Git")))
        (("r" . "DropBox")  . (lambda () (interactive) (dired-x-find-file "~/Dropbox")))
        ))

(defun one-key-menu-dir ()
  "The `one-key' menu alist for dir."
  (interactive)
  (one-key-menu "Open a Dir" one-key-menu-dir-alist))

;; Font size adjustment
(defvar one-key-menu-font-size-alist nil
  "The `one-key' menu list for Font-SIZE.")

(setq one-key-menu-font-size-alist
      '((("=" . "Increase font size") . increase-setup-font-size)
        (("-" . "Decrease font size") . decrease-setup-font-size)
        (("0" . "Default font size")  . default-setup-font-size)))

(defun one-key-menu-font-size ()
  "The `one-key' menu for Font-SIZE."
  (interactive)
  (if (and (fboundp 'increase-setup-font-size) (display-graphic-p))
      (one-key-menu "FONT-SIZE" one-key-menu-font-size-alist nil t)
    (message "Please run %s in the graphical interface." "one-key-menu-font-size")))

(provide 'lex-one-key)

;;; lex-one-key.el ends here
