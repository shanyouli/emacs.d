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

;;
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
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* (;; Force straight to install into ~/.emacs.d/package/straight instead of
         ;; ~/.emacs.d/straight by pretending `lye-emacs-package-dir' is our .emacs.d.
         (user-emacs-directory straight-base-dir)
         (bootstrap-file (lib-f-join straight-base-dir "straight"
                                     "repos" "straight.el" "bootstrap.el"))
         (bootstrap-version 5))
    (lib-f-make-dir (lib-f-join straight-base-dir "straight/build"))
    (unless (featurep 'straight)
      (unless (or (require 'straight nil t)
                  (file-readable-p bootstrap-file))
        (with-current-buffer
            (url-retrieve-synchronously
             (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
                     straight-repository-branch)
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil t))))

(defun straight-initialize-packages (&optional package-init-notp straight-init-notp)
  "Initialize `package' and `straight',
If PACKAGE-INIT-NOTP are non-nil, then `package.el' is not initialized.
If STRAIGHT-INIT-NOTP are non-nil, then `straight.el' is not initialized."
  (unless package-init-notp
    (message "Initializing package...")
      (setq lpm-package-dir (concat lye-emacs-cache-dir "lpm/"))
      (setq lpm-recipe-alist
            '((vterm . (:type git :host github :repo "akermu/emacs-libvterm"))
              (fuz . (:type git :host github :repo "cireu/fuz.el"))
              (ivy-fuz . (:pseudo fuz))
              (pdf-tools . (:repo "politza/pdf-tools")))))
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

(defalias 'package+ 'straight-use-package)

(straight-initialize-packages)


;; use package

;; 安装一些必须的packages
(defvar core--base-package
  '(use-package
    bind-key
    s
    async
    f
    (org :type built-in) ;Org avoid installation package again.
    dash))
(dolist (pkg core--base-package)
  (package+ pkg))

(eval-when-compile (require 'use-package))

(provide 'core-straight)

;;; core-straight.el ends here
