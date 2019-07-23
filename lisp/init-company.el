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
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (setq lye-company-enable-yas t)

  (defvar company-enable-yas lye-company-enable-yas
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)

    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :bind (
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)

  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq company-idle-delay 0
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

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

;; Popup documentation for completion candidates
(when (display-graphic-p)
  (use-package company-quickhelp
    :bind (:map company-active-map
           ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :config (setq company-quickhelp-delay 0.8)))

;; Use company-box
(when (display-graphic-p)
  (use-package company-posframe
    :after company))

(provide 'init-company)
;;; init-company.el ends here
