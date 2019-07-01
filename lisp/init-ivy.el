;;; init-ivy.el --- Initialize ivy configurations.   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>
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

;; Ivy configuration

;;; Code:

(use-package counsel
  :ensure nil
  :diminish ivy-mode counsel-mode
  :bind (("C-x f" . counsel-recentf)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ([escape] . minibuffer-keyboard-quit)
         ("<C-return>" . ivy-immediate-done))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t) ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function #'ivy-format-function-arrow) ; format-function >
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-fuzzy))) ; Fuzzy search
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; Use faster search tools: rigprep
  (let ((command
     (cond
      ((executable-find "rg")
       "rg -i M 120 --no-beading --line-number --color never '%s' %s"))))
    (setq counsel-grep-base-command command)))

;;Ivy is more intelligent
(use-package amx
  :after counsel
  :config
  (setq amx-save-file (concat lye-emacs-cache-dir "amx-items"))
  (setq amx-history-length 10)
  (amx-initialize))

(provide 'init-ivy)
;;; init-ivy.el ends here
