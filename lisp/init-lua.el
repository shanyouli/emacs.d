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
(package! '(lua-mode :type git :host github
                    :repo "immerrr/lua-mode"))
(use-package lua-mode
  :ensure nil
  :defer t
  :mode (("\\.lua\\'" . lua-mode)
         (".luacheckrc" . lua-mode)
         ("rc.lua.template" . lua-mode))
  :config
  (setq lua-indent-level 4
        lua-indent-string-contents nil))
(use-package company-lua :straight t)

(add-hook 'lua-mode-hook
          (lambda ()
            (lye/exec-path-from-shell-init)
            (when (executable-find "luacheck")
              (lye/modules-require 'iex-flycheck)
              (flycheck-mode +1))
            (setq-local company-backends
                        (cons '(company-lua company-tabnine company-yasnippet)
                              company-backends))))

(provide 'init-lua)
;;; init-lua.el ends here
