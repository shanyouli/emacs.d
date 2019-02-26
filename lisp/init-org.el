;;; init-org.el ---Org-mode configurations.          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>
;; Keywords:

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

(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-switchb))
  :mode ("\\.org\\'" . org-mode)
  :ensure nil
  :config
  ;;Align tag
  ;; (dolist (face '(org-level-1
  ;;   	  org-level-2
  ;;   	  org-level-3
  ;;   	  org-level-4
  ;;   	  org-level-5
  ;;   	  org-level-6
  ;;   	  org-level-7))
  ;;   (set-face-attribute face nil :height 1.0))
  (add-hook 'org-mode-hook
            '(lambda ()
               (auto-fill-mode nil) ; 不自动换行
               (setq truncate-lines nil) ; 自动换行
               (if (display-graphic-p)
                   (use-package org-bullets
                     :init
                     (org-bullets-mode 1))
                 (org-indent-mode t) ; 自动缩进, * 和 ** etc.
                 ))))

(provide 'init-org)
;;; init-org.el ends here
