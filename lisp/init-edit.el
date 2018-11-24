;;; init-edit.el ---Initialize Edit Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye liivy-alt-done <shanyouli6@gamil.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;Brackets highlighted
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (after-init . (lambda ()
			(global-highlight-parentheses-mode))))

;; Pair Automatic completion
(use-package autopair
  :diminish autopair-mode
  :hook (after-init . autopair-global-mode))

;;Chinese input automatically adds spaces in Chinese
(use-package pangu-spacing
  :diminish pangu-spacing-mode
  :hook (after-init . global-pangu-spacing-mode))

;;Big blank delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode))

;; Displays line-number.el
(dolist (hook (list
	       'c-mode-common-hook
	       'emacs-lisp-mode-hook
	       'sh-mode-hook
	       'org-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode))))

;; Don't display `symbolic link to Git-controlled source file....'
;; @see https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks nil)

(provide 'init-edit)
;;; init-edit.el ends here
