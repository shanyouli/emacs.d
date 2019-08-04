;;; init-sh.el --- Shell Script Configurations -*- lexical-binding: t -*-

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
             (let ((file-name (buffer-file-name)))
               (if (and file-name (or (string-match "\\.zsh$" file-name)
                                      (string-match "\\.zshrc$" file-name)
                                      (string-match "\\.zshenv$" file-name)
                                      (string-match "\\.zlogin$" file-name)
                                      (string-match "\\.zshfunc$" file-name)))
                   (sh-set-shell "zsh")
                 (sh-set-shell "bash")))

             (when (and (string= sh-shell "bash")
                        (executable-find "bash-language-server"))
               (lye/modules-require 'iex-lsp)
               (lsp)
               (setq-local company-backends
                           (cons '(company-tabnine company-lsp)
                                 company-backends))
               ;; (setq-local company-backends
               ;;             (mapcar #'company-backend-with-yas company-backends))
               (lye/modules-require 'iex-flycheck)
               ))))

(provide 'init-sh)
;;; init-sh.el ends here
