;;; md-elisp.el --- Iniliatize Emacs Lisp Configurations  -*- lexical-binding: t; -*-

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
          ("C-c C-b" . eval-buffer))
  :hook (emacs-lisp-mode  . (lambda ()
                              "Beautify emasc-lisp"
                              (push '("lambda" .  "λ") prettify-symbols-alist)
                              (prettify-symbols-mode)))
  :config
  (if (boundp 'elisp-flymake-byte-compile-load-path)
      (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Add remove buttons for advices
  (add-hook 'help-mode-hook 'cursor-sensor-mode)

  (defun function-advices (function)
    "Return FUNCTION's advices."
    (let ((function-def (advice--symbol-function function))
          (ad-functions '()))
      (while (advice--p function-def)
        (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
        (setq function-def (advice--cdr function-def)))
      ad-functions))

  (define-advice describe-function-1 (:after (function) advice-remove-button)
    "Add a button to remove advice."
    (when (get-buffer "*Help*")
      (with-current-buffer "*Help*"
        (save-excursion
          (goto-char (point-min))
          (let ((ad-index 0)
                (ad-list (reverse (function-advices function))))
            (while (re-search-forward "^:[-a-z]+ advice: \\(.+\\)$" nil t)
              (let* ((name (string-trim (match-string 1) "'" "'"))
                     (advice (or (intern-soft name) (nth ad-index ad-list))))
                (when (and advice (functionp advice))
                  (let ((inhibit-read-only t))
                    (insert "\t")
                    (insert-text-button
                     "[Remove]"
                     'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                     'help-echo (format "%s" advice)
                     'action
                     ;; In case lexical-binding is off
                     `(lambda (_)
                        (when (yes-or-no-p (format "Remove %s ? " ',advice))
                          (message "Removing %s of advice from %s" ',function ',advice)
                          (advice-remove ',function ',advice)
                          (revert-buffer nil t)))
                     'follow-link t))))
              (setq ad-index (1+ ad-index))))))))

  ;; Remove hook
  (defun remove-hook-at-point ()
    "Remove the hook at the point in the *Help* buffer."
    (interactive)
    (unless (or (eq major-mode 'help-mode)
                (string= (buffer-name) "*Help*"))
      (error "Only for help-mode"))
    (let ((orig-point (point)))
      (save-excursion
        (when-let
            ((hook (progn (goto-char (point-min)) (symbol-at-point)))
             (func (when (and
                          (or (re-search-forward (format "^Value:[\s|\n]") nil t)
                              (goto-char orig-point))
                          (sexp-at-point))
                     (end-of-sexp)
                     (backward-char 1)
                     (catch 'break
                       (while t
                         (condition-case _err
                             (backward-sexp)
                           (scan-error (throw 'break nil)))
                         (let ((bounds (bounds-of-thing-at-point 'sexp)))
                           (when (< (car bounds) orig-point (cdr bounds))
                             (throw 'break (sexp-at-point)))))))))
          (when (yes-or-no-p (format "Remove %s from %s? " func hook))
            (remove-hook hook func)
            (revert-buffer nil t))))))
  (bind-key "C-c d" #'remove-hook-at-point help-mode-map))

;; Show function arglist or variable docstring
;; `global-edloc-mode' is enabled by default.

;; Interacitve macro expander
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
          ("C-c e" . macrostep-expand)
          :map lisp-interaction-mode-map
          ("C-c e" . macrostep-expand)))

;; Semantic code search for emacs lisp
(use-package elisp-refs :ensure t)

;; Function that highlights global variables
(package! '(elispfl :type git :host github
                    :repo "cireu/elispfl"))
(require 'elispfl)
(elispfl-mode +1)

(package! '(sly-el-indent
            :type git
            :host github
            :repo "cireu/sly-el-indent"
            :files (:defaults "lib")
            :no-byte-compile t))
(require 'sly-el-indent)
(add-hook 'emacs-lisp-hook
            (function sly-el-indent-setup))

(provide 'md-elisp)
;;; md-elisp.el ends here