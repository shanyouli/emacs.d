;;; init-package.el --- Initialize package configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
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

;; Emacs Package Management configurations.

;;; Code:

;; Use the more modern package management menu paradox
(use-package paradox
  :ensure t
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)

  (defalias 'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defadvice list-packages (before my-list-packages activate)
    (paradox-enable)))

(provide 'init-package)
;;; init-package.el ends here
