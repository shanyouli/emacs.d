;;; init-fci.el --- Fill Column Initialize -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.01
;; Package-Requires: (fill-column-indicator column-enforce-mode)
;; Homepage: homepage
;; Keywords: 80-columns


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

;; 80-columns

;;; Code:

;;; Variable

(defcustom lye-use-column-enforce-mode nil
  "Whether to use the column-enforce-mode package."
  :type 'boolean)

(defcustom lye-use-fci-mode t
  "Whether to use the FCI-MODE package."
  :type 'boolean)

;;; Functions

(defun lye/use-column-enforce-mode ()
  "enable `column-enforce-mode'."
  (when lye-use-column-enforce-mode
    (use-package column-enforce-mode
      :custom-face
      (column-enforce-face
       ((t :inherit nil :foreground "OrangeRed")))
      :hook (after-init . global-column-enforce-mode)
      :config
      (setq column-enforce-comments nil) ;Ignore long notes
      (setq column-enforce-column 80))))

;; Display compatibility issue with company
;; @see https://github.com/alpaker/fill-column-indicator/issues/54#issuecomment-218344694
(defun on-off-fci-before-company (command)
  "Fix conflict between fci-mode and company-mode."
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

;; Conflict with fci-mode
;; @see https://github.com/tumashu/pyim/issues/205#issuecomment-386872146
(defun lye/pyim-tooltip-show (orig-func string position)
  "pyim turn off `fci-mode'."
  ;;(message "turn off fci")
  (funcall orig-func string position))

(defun lye/pyim-terminate-translation (orig-func)
  ;;(message "turn on fci")
  (funcall orig-func))

(defun lye/use-fci-mode ()
  "enable `fci-mode'."
  (when lye-use-fci-mode
    (use-package fill-column-indicator
      :ensure t
      :hook ((prog-mode . fci-mode)
             ((magit-mode esup-mode) . (lambda () (fci-mode -1))))
      :init
      (setq fci-rule-column 80)
      (setq fci-rule-width 1)
      :custom  ;; Avoid conflicts with fci-rule-color defined in doom-themes
      (fci-rule-color "orange") ;; (custom-set-variables '(fci-rule-color "orange"))
      )))

;;; Configurations

(add-hook
 'after-init-hook
 (lambda ()
   ;; enable fci-mode
   (lye/use-fci-mode)
   (when (locate-library "company")
     (advice-add 'company-call-frontends :before #'on-off-fci-before-company))
   (when (locate-library "pyim")
     (advice-add 'pyim-tooltip-show :around #'lye/pyim-tooltip-show)
     (advice-add 'pyim-terminate-translation
                 :around #'lye/pyim-terminate-translation))
   ;; enable column-enforce-mode
   (lye/use-column-enforce-mode)))

(provide 'init-fci)

;;; init-fci.el ends here
