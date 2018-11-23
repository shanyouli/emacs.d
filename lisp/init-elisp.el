;;; init-elisp.el --- Iniliatize Emacs Lisp Configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>

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

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-x" . ielm)
	      ("C-c C-c" . eval-defun)
	      ("C-c C-b" . eval-buffer)))

;; Show function arglist or variable docstring
;; `global-edloc-mode' is enabled by default.

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Interacitve macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
	      ("C-c e" . macrostep-expand)
	      :map lisp-interaction-mode-map
	      ("C-c e" . macrostep-expand)))

;; Semantic code search for emacs lisp
(use-package elisp-refs)

;; A better *Help* buffer
(use-package helpful
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
		   helpful-variable
		   helpful-function
		   helpful-macro
		   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist))))


(provide 'init-elisp)
;;; init-elisp.el ends here
