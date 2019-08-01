;;; init-python.el ---Initialize python              -*- lexical-binding: t; -*-

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

(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :config
    ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))

  ;; Live Coding in Python
  (use-package live-py-mode)

  (use-package pyenv-mode)
  (use-package pyenv-mode-auto))

;; Format using YAPF
;; Install: pip install yapf
(when (executable-find "yapf")
  (use-package yapfify
    :commands (yapf-mode)
    :hook (python-mode . yapf-mode)))

(add-hook 'python-mode-hook
          (lambda () (require 'pyenv-mode-auto)
            (when (executable-find "pyls")
               (lye/modules-require 'iex-lsp)
               (lsp)
               (setq-local company-backends
                           (mapcar #'company-backend-with-yas company-backends))
               )))

(provide 'init-python)
;;; init-python.el ends here
