;;; iex-lsp.el --- Language Server Progra Configurations -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (lsp-mode lsp-ui company-lsp)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: lsp


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

;; Language Server Program(LSP) Configuraions

;;; Code:

;; Installed packages
(require 'lsp-mode)
(require 'lsp-ui)
(require 'company-lsp)


;; lsp-mode Configurations
(setq lsp-auto-guess-root nil) ; Detect project root
(setq lsp-prefer-flymake nil) ; Use lsp-ui and flycheck
(setq-default lsp-session-file (concat lye-emacs-cache-dir "lsp-session"))
;; see @https://github.com/emacs-lsp/lsp-mode/issues/641
(setq lsp-restart 'auto-restart) ; auto-pyls

;;lsp-ui Configurations
(setq lsp-ui-doc-enable t
      lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-position 'top
      lsp-ui-doc-use-webkit t
      lsp-ui-doc-border (face-foreground 'default)
      lsp-ui-sideline-enable nil
      lsp-ui-sideline-ignore-duplicate t)

(provide 'iex-lsp)

;;; iex-lsp.el ends here
