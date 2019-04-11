;;; init-package.el --- Initialize package configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
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

;; Emacs Package Management configurations.

;;; Code:

(require 'package)
;; HACK: DO NOT save the variable "package-selected-packages" in init/custom file
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set and (don't!) save 'package-selected-packages' to VALUE"
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; ELPA: refer to https://melpa.org and https://elpa.emacs-china.org
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository"
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             '(melpa melpa-mirror emacs-china netease)))))
  (let* ((no-ssl (and (memq system-type '(window-nt ms-dos))
                     (not (gnutls-available-p))))
        (proto (if no-ssl "http" "https")))
    (cond
     ((eq archives 'melpa)
      (setq package-archives `(,(cons "gnu"  (concat proto "://elpa.gnu.org/packages/"))
                               ,(cons "melpa" (concat proto "://melpa.org/packages/")))))
     ((eq archives 'melpa-mirror)
      (setq package-archives `(,(cons "gnu"  (concat proto "://elpa.gnu.org/packages/"))
                               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")))))
     ((eq archives 'emacs-china)
      (setq package-archives `(,(cons "gnu"  (concat proto "://elpa.emacs-china.org/gnu/"))
                               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")))))
     ((eq archives 'netease)
      (setq package-archives `(,(cons "gnu"  (concat proto "://mirrors.163.com/elpa/gnu/"))
                               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/")))))
     (t
      (error "Unknown archives: '%s'" archives))))
  (message "Set package archives to '%s' ." archives))
;; Set package archives
(set-package-archives lye-package-archives)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil) ;To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(dolist (package '(
                   use-package
                   diminish
                   bind-key))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))
;; Should set before loading 'use-packge'
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-process-buffer " *straight-process*")

(eval-when-compile (require 'use-package))

;; Extensions


(provide 'init-package)
;;; init-package.el ends here
