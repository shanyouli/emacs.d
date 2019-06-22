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

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root nil) ; Detect project root
  (setq lsp-prefer-flymake nil) ; Use lsp-ui and flycheck
  (setq lsp-session-file (concat lye-emacs-cache-dir "lsp-session"))
  :config
  ;; see @https://github.com/emacs-lsp/lsp-mode/issues/641
  (setq lsp-restart 'auto-restart) ; auto-pyls
  (use-package lsp-ui
    :commands lsp-ui-mode
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references)
                ("C-c u" . lsp-ui-imenu))
    :init
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-header t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-position 'top
          lsp-ui-doc-use-webkit t
          lsp-ui-doc-border (face-foreground 'default)
          lsp-ui-sideline-enable nil
          lsp-ui-sideline-ignore-duplicate t))
  (use-package company-lsp :commands company-lsp))



;; (add-hook 'sh-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(provide 'init-lsp)
;;; init-lsp.el ends here
