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
      ;;      straight-cache-autoloads nil
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t
      straight-process-buffer " *straight-process*")

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

(defun straight-initialize-packages ()
  "Initialize straight"
  (message "Initializing straight")
  (doom-ensure-straight)
  (require 'straight)
  (mapc #'straight-use-recipes straight-core-package-sources))

(defun package! (pkg-name &optional loadp use-straight-p)
  "Install a package"
  (if (or use-straight-p
          (consp pkg-name))
      (straight-use-package pkg-name)
    (require-package pkg-name))

  (when loadp
    (let ((package-name (if (consp pkg-name)
                            (car pkg-name)
                          pkg-name)))
      (require package-name))))

(straight-initialize-packages)

;; use package
(package! 'use-package t t)

(use-package bind-key
  :straight t)


;; some useful library
(package! 's nil t)

(provide 'core-straight)

;;; core-straight.el ends here
