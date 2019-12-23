;;; core-straight.el --- Init Next package Management -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (straight)
;; Homepage: https://github.com/shanyou/emacs.d
;; Keywords: package Management


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

;; Next package Management

;;; Code:

;;; package management

;;
;;; package.el, 配置
(defcustom core-pkg-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa-mirror" melpa-mirror)
          (const :tag "Emacs-china" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)
          (const :tag "Tencent" tencent)))

;; HACK: Do not save the variable "Package-selected-packages" in custom file
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun core-pkg-save-selected (&optional value)
  "Set and (don't save `package-selected-packages' to VALUE.)"
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

;; ELPA: refer to https://melpa.org and https://elpa.emacs-china.org
;;;###autoload
(defun core-pkg-set-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china
                                          netease tencent tuna)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            ('tencent
             `(,(cons "gnu" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
            (archives
             (error "Unknown archives: `%s'" archives)))))

  (message "Set package archives to `%s'." archives))

(defun core-pkg-initialize (&optional save-p melpa-archive)
  (let ((melpa-archive (or melpa-archive core-pkg-archives)))
    (core-pkg-set-archives melpa-archive)
    (unless save-p
      (advice-add 'package--save-selected-packages
                  :override #'core-pkg-save-selected))))

;; Set the location where the elpa folder is stored
(setq package-user-dir (concat lye-emacs-cache-dir "elpa"))

(setq core-pkg-archives 'tuna)
(core-pkg-initialize t)

;;; straight.el
(defvar straight-core-package-sources
  '((org-elpa :local-repo nil)
    (melpa :type git
           :host github
           :repo "melpa/melpa")
    (gnu-elpa-mirror :type git
                     :host github
                     :repo "emacs-straight/gnu-elpa-mirror")
    (emacsmirror-mirror :type git
                        :host github
                        :repo "emacs-straight/emacsmirror-mirror")))

;; straight
(setq straight-base-dir lye-emacs-cache-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil  ; use-autoload
      ;; straight-disable-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t
      straight-process-buffer " *straight-process*" ; hide *straight-process*
      straight-check-for-modifications nil
      straight-build-dir (concat straight-base-dir "straight/build"))

(defun doom-ensure-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun straight-initialize-packages (&optional package-init-notp straight-init-notp)
  "Initialize `package' and `straight',
If PACKAGE-INIT-NOTP are non-nil, then `package.el' is not initialized.
If STRAIGHT-INIT-NOTP are non-nil, then `straight.el' is not initialized."
  (unless package-init-notp
    (message "Initializing package...")
    (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
      (setq package-enable-at-startup nil) ;To prevent initializing twice
      (setq lpm-package-dir (concat lye-emacs-cache-dir "lpm/"))
      (setq lpm-recipe-alist
            '((vterm . (:type git :host github :repo "akermu/emacs-libvterm"))
              (fuz . (:type git :host github :repo "cireu/fuz.el"))
              (ivy-fuz . (:pseudo fuz))
              (pdf-tools . (:repo "politza/pdf-tools"))))
      (require 'modules-package)
      (lpm-initialize)
      (package-initialize)
      ;; (lib-package-ext-initialize)
      ))
  (unless straight-init-notp
    (message "Initializing straight...")
    (doom-ensure-straight)
    (require 'straight)
    (mapc #'straight-use-recipes straight-core-package-sources)))

(defun switch-to-straight-buffer ()
  "Open the `*straight-process*'."
  (interactive)
  (let* ((straight-buffer straight-process-buffer)
        (blist (mapcar #'buffer-name (buffer-list))))
    (if (and straight-buffer (member straight-buffer blist))
        (switch-to-buffer straight-buffer))))

(defmacro package+ (pkg-name)
  "install a package"
  `(if (listp ,pkg-name)
      (let* ((pkg (if (= 1 (length ,pkg-name))
                           (car ,pkg-name)
                         ,pkg-name)))
        (straight-use-package pkg))
     (lpm-install ,pkg-name)))
(defun package++ (package &optional straight)
  "Install a package with `straight-use-package' or `lpm-install'."
  (if (or (listp package)
          straight)
      (straight-use-package package)
    (lpm-install package)))

(defmacro package! (pkg-name &rest args)
  "Install a package. and add some configuration."
  (declare (indent defun))
  (let ((-if (plist-get+ args :if t))
        (-unless (plist-get+ args :unless nil))
        (-straight (plist-get+ args :straight (listp pkg-name)))
        (name (intern (format "lye-package--use-%s-p"
                              (symbol-name (if (listp pkg-name)
                                               (car pkg-name)
                                             pkg-name))))))
    `(progn
       (defvar ,name ,-if)
       (if ,-if
           (package++ ,pkg-name ,-straight)
         (when ',-unless
           (if (listp ',-unless)
               (package++ (car ',-unless) (cadr ',-unless))
             (package++ ,-unless)))))))

(straight-initialize-packages)


;; use package

;; 安装一些必须的packages
(defvar core--base-package
  '((use-package)
    (bind-key)
    (s)
    (async)
    (f)
    (org :type built-in) ;Org avoid installation package again.
    (dash)))

(dolist (pkg core--base-package)
  (package+ pkg))

(eval-when-compile (require 'use-package))

;; pardox
(run-with-idle-timer 5 nil (lambda () (lye/modules-require 'iex-paradox)))

(provide 'core-straight)

;;; core-straight.el ends here
