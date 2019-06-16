;;; init-lua.el ---Initialize Lua Language           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:languages

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

(use-package lua-mode
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
 ;; :interpreter (.lua . lua-mode)
  :config
  (setq lua-indent-level 2
        lua-indent-string-contents t))

(use-package company-lua
  :after (company))

(defun set-company-backends-for-lua ()
    "Set lua company backend."
  (setq-local company-backends '(
                                (company-lsp
                                 company-lua
                                 company-keywords
                                 company-gtags)
                                company-capf
                                company-dabbrev-code
                                company-files
                                )))

(add-hook 'lua-mode-hook #'set-company-backends-for-lua)

(provide 'init-lua)
;;; init-lua.el ends here
