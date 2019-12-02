;;; md-python.el ---Initialize python              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords: languages

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

;; Format using YAPF
;; Install: pip install yapf
(when (executable-find "yapf")
  (use-package yapfify
    :commands (yapf-mode)
    :hook (python-mode . yapf-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            (when (locate-library "pyenv-mode-auto")
              (require 'pyenv-mode-auto))
            (when (locate-library "lsp-python-ms")
              (require 'lsp-python-ms))
            (setq lsp-session-file (concat lye-emacs-cache-dir "lsp-session" ))
            (lsp)
            (setq-local company-backends
                        (mapcar #'company-backend-with-yas company-backends))))

(provide 'md-python)
;;; md-python.el ends here
