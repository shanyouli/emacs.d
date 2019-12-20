;;; md-lua.el ---Initialize Lua Language           -*- lexical-binding: t; -*-

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
  :ensure nil
  :defer t
  :mode (("\\.lua\\'" . lua-mode)
         (".luacheckrc" . lua-mode)
         ("rc.lua.template" . lua-mode))
  :config
  (setq lua-indent-level 4
        lua-indent-string-contents nil))

(with-eval-after-load 'lua-mode
  (if (executable-find "luacheck")
      (lye/modules-require 'iex-flycheck))

  ;; lsp
  (require 'lsp-lua-emmy)
  (require 'iex-lsp)
  (setq lsp-lua-emmy-jar-path
        (expand-file-name "~/.local/share/jar/emmylua/EmmyLua-LS-all.jar")))

(add-hook 'lua-mode-hook
          (lambda ()
            (when (executable-find "luacheck")
              (flycheck-mode +1))
            (lsp)
            (setq-local company-backends
                        (cons '(company-lsp company-lua company-yasnippet)
                              company-backends))))

(provide 'md-lua)
;;; md-lua.el ends here
