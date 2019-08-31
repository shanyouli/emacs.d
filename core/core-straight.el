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

;; (defun doom-ensure-straight ()
;;   "Ensure `straight' is installed and was compiled with this version of Emacs."
;;   (defvar bootstrap-version)
;;   (let* (;; Force straight to install into ~/.emacs.d/.local/straight instead of
;;          ;; ~/.emacs.d/straight by pretending `doom-local-dir' is our .emacs.d.
;;          (user-emacs-directory straight-base-dir)
;;          (bootstrap-file (concat straight-base-dir "straight/repos/straight.el/straight.el"))
;;          (bootstrap-version 5))
;;     (make-directory (concat straight-base-dir "straight/build") 'parents)
;;     (unless (featurep 'straight)
;;       (unless (or (require 'straight nil t)
;;                   (file-readable-p bootstrap-file))
;;         (with-current-buffer
;;             (url-retrieve-synchronously
;;              "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;              'silent 'inhibit-cookies)
;;           (goto-char (point-max))
;;           (eval-print-last-sexp)))
;;       (load bootstrap-file nil t))))

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
  (mapc #'straight-use-recipes straight-core-package-sources)
  (straight-register-package
   `(straight :type git :host github
              :repo ,(format "%s/straight.el" straight-repository-user)
              :files ("straight*.el")
              :branch ,straight-repository-branch)))

(straight-initialize-packages)

;; use package
(straight-use-package 'use-package)
(require 'use-package)
;;(setq straight-use-package-by-default t)

(use-package bind-key
  :straight t)

(provide 'core-straight)

;;; core-straight.el ends here
