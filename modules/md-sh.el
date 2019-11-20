;;; md-sh.el --- Shell Script Configurations -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (sh-script)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: sh


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

;; commentary

;;; Code:

;; sh-mode
(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
         (".zshrc" . sh-mode))
  :hook
  (sh-mode .
           (lambda ()
             ;; Determine if the shell using the running script is zsh or bash
             (let*  ((f (buffer-file-name))
                     (f (if f (file-name-nondirectory f) nil)))
               (if (and f (or (string-match "^\\.zsh\\(\\rc\\|\\env\\|func\\)$" f)
                              (string-match "\\.zsh$" f)
                              (string-match "^\\.zlogin$" f)))
                   (sh-set-shell "zsh")
                 (sh-set-shell "bash")))

             ;; run flycheck
             (lye/modules-require 'iex-flycheck)
             (flycheck-mode +1)
             ;; run lsp
             (when (and (string= sh-shell "bash")
                        (executable-find "bash-language-server"))
               (lye/modules-require 'iex-lsp)
               (lsp)
               (setq-local company-backends
                           (cons '(company-tabnine company-lsp)
                                 company-backends))
               ;; (setq-local company-backends
               ;;             (mapcar #'company-backend-with-yas company-backends))
               (add-hook 'lsp--managed-mode-hook (lambda () (eldoc-mode -1)))))))

(provide 'md-sh)
;;; md-sh.el ends here
