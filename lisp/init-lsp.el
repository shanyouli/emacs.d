;;; init-lsp.el ---Language Server Program Configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
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

;; Language Server Progaram Configuraions

;;; Code:

(use-package lsp
  :ensure lsp-mode
  :diminish lsp-mode
  :hook ((json-mode . lsp)
         (lsp-after-open . lsp-enable-imenu)))


(provide 'init-lsp)
;;; init-lsp.el ends here
