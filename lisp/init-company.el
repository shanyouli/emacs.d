;;; init-company.el --- Initialize Company configurations.  -*- lexical-binding: t; -*-

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

;; Company Configuration

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defun company-backend-with-yas (backend)
    "Use `company-yasnippet' as the complement backend for `company'."
    (if (or (not lye-company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :bind (:map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)

  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq company-idle-delay 0.3
        compant-echo-delay (if (display-graphic-p) nil 0)
        company-tooltip-limit 10
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-show-numbers t)

  ;; Avoid entering code blocks quickly in org-mode because the prompts don't respond
  (setq company-minimum-prefix-length 2)

  ;;Do not use it in these major modes
  (setq company-global-modes
        '(not message-mode git-commit-mode))

  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend))

  ;; Use company-tabnine
  (unless (or system/windows (executable-find "wsl.exe"))
    (use-package company-tabnine
      :ensure t
      :init
      (setq company-tabnine-binaries-folder
            (expand-file-name "TabNine" lye-emacs-share-dir))

      (add-to-list 'company-backends #'company-tabnine)
      :config
      (defun tabnine-start-advice (orig &rest rest)
        (with-temp-message
            (with-current-buffer " *Minibuf-0*" (buffer-string))
          (let ((inherit-message t))
            (apply orig rest))))
      (advice-add 'company-tabnine-start-process :around 'tabnine-start-advice)
      ;; The free version of TabNine is good enough,
      ;; and below code is recommended that TabNine not always
      ;; prompt me to purchase a paid version in a large project.
      (defun company-disable-tabnine-upgrade-message (orig args)
        (when (and args
                   (stringp (funcall args)))
          (unless (string-match "The free version of TabNine only indexes up to"
                                (funcall args))
            (apply orig args))))
      (advice-add 'company-echo-show :around 'company-disable-tabnine-upgrade-message)))

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

;; Use company-posframe
(with-eval-after-load 'company
  (when (display-graphic-p)
    (package! 'company-posframe t)
    (company-posframe-mode +1)))

(provide 'init-company)
;;; init-company.el ends here
