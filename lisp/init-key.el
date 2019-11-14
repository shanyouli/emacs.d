;;; init-key.el --- package.el keywords -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: ()
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

;; Key

;;; Code:

;; iex-ivy.el
(mdk/set-keys!
 '(("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x f"   . counsel-recentf)
   ("C-s"     . swiper-isearch)
   ("C-z s t" . counsel-load-theme)
   ("M-y"     . counsel-yank-pop)
   ("C-x b"   . ivy-switch-buffer)
   ("C-x d"   . counsel-dired))
 nil nil "iex-ivy")

;; iex-elfeed
(mdk/set-keys! '(("C-z w" . elfeed)) nil nil "iex-elfeed")

;; iex-treemacs
(mdk/set-keys! '(("C-x t" . one-key-menu-treemacs)) nil nil "iex-treemacs")

;; iex-git
;; transient file
(setq-default transient-history-file
              (concat lye-emacs-cache-dir "transient/history.el"))
(setq transient-values-file
      (concat lye-emacs-cache-dir "transient/values.el"))
(setq transient-levels-file
      (concat lye-emacs-cache-dir "transient/levels.el"))
;; Forge configuration
(setq forge-database-file
      (expand-file-name "forge-database.sqlite" lye-emacs-cache-dir))
(mdk/set-keys! '(("C-x g" . one-key-menu-magit)) nil nil "iex-git")

;; iex-window
(mdk/set-keys! '(("C-x 4 u" . winner-undo)
                 ("C-x 4 r" . winner-redo)) nil nil "iex-window")

;; iex-avy
(mdk/set-keys! '(("M-s" . one-key-avy/menu)) nil nil "iex-avy")

;; iex-vterm
(mdk/set-keys! '(("C-x s v" . term-toggle)) nil nil "iex-term")

;; iex-pomidor.el
(mdk/set-keys! '(("C-z s c" . pomidor)) nil nil "iex-pomidor")

;; open line in browser
;; see @https://github.com/noctuid/link-hint.el/
(md-pkg/install+ 'link-hint)
(mdk/set-keys!
 '(("C-x p o" . link-hint-open-link)
   ("C-x p c" . link-hint-copy-link))
 nil nil  "link-hint")

(md-pkg/install+ 'org-cliplink)
(mdk/set-keys! '(("C-x p i" . org-cliplink)) nil nil "org-cliplink")

;; lex-snails
(when (and (not system/windows) (display-graphic-p))
  (md-key/unset-keys+ "C-x C-b")
  (mdk/set-keys! '(("C-x C-b" . snails)
                   ("C-z C-s" . snails-load-theme))  nil nil "iex-snails"))

;; Better elisp help file format
;; (md-pkg/install+ 'helpful)
;; (mdk/set-keys!
;;  '(("C-h k" . helpful-key)
;;    ("C-h d" . helpful-at-point))
;;  "helpful")

;; iex-tldr
(unless system/windows
  (mdk/set-keys! '(("C-z s h" . tldr))  nil nil "iex-tldr"))

;; iex-smart-align
(mdk/set-keys! '(("C-z s m" . smart-align)) nil nil "iex-smart-align")
(provide 'init-key)

;;; init-key.el ends here
