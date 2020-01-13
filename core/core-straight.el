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
;;; straight.
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

(defvar lye-build-in-packags '((org :type built-in)
                               (pyim :type built-in)
                               (async :type built-in)
                               (xr :type built-in)
                               (pyim-basedict :type built-in)
                               (doom-themes :type built-in)
                               (noflet :type built-in)
                               (super-save :type built-in)))

(defvar lye-core-packages '(straight use-package)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

;; straight
(setq straight-base-dir lye-emacs-cache-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil  ; use-autoload
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom refresh' instant (once everything set up), which is much
      ;; nicer UX than the several seconds modification checks.
      ;; straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      ;; straight-enable-package-integration nil
      ;; straight-disable-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t
      straight-process-buffer " *straight-process*" ; hide *straight-process*
      straight-check-for-modifications nil
      straight-build-dir (concat straight-base-dir "straight/build")
      straight-dynamic-modules-dir (concat straight-base-dir "dynamic-modules/"))

(defun lye-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* (;; Force straight to install into ~/.emacs.d/package/straight instead of
         ;; ~/.emacs.d/straight by pretending `lye-emacs-package-dir' is our .emacs.d.
         (user-emacs-directory straight-base-dir)
         (bootstrap-file (lib-f-join straight-base-dir "straight"
                                     "repos" "straight.el" "bootstrap.el"))
         (bootstrap-version 5))
    (lib-f-make-dir straight-build-dir)
    (lib-f-make-dir straight-dynamic-modules-dir)
    (lib-load-add-load-path straight-dynamic-modules-dir)
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

(defun straight-initialize-packages (&optional straight-init-notp)
  "Initialize `package' and `straight',
If STRAIGHT-INIT-NOTP are non-nil, then `straight.el' is not initialized."
  (unless straight-init-notp
    (message "Initializing straight...")
    (unless (fboundp 'straight--reset-caches)
      (lye-ensure-straight)
      (require 'straight))
    ;; (straight--reset-caches)
    ;; (setq straight-recipe-repositories nil
          ;; straight-recipe-overrides nil)
    (mapc #'straight-use-recipes straight-core-package-sources)
    ;; (straight-register-package
    ;;  `(straight :type git :host github
    ;;             :repo ,(format "%s/straight.el" straight-repository-user)
    ;;             :files ("straight*.el")
    ;;             :branch ,straight-repository-branch
    ;;             :no-byte-compile t))
    (mapc #'straight-use-package lye-build-in-packags)
    (mapc #'straight-use-package lye-core-packages)))

(defun switch-to-straight-buffer ()
  "Open the `*straight-process*'."
  (interactive)
  (let* ((straight-buffer straight-process-buffer)
        (blist (mapcar #'buffer-name (buffer-list))))
    (if (and straight-buffer (member straight-buffer blist))
        (switch-to-buffer straight-buffer))))

(defalias 'package+ 'straight-use-package)

(defmacro package! (name &rest args)
  "Install a package-name.

Usage:

    (package! NAME
        [:keyword [option]])
:if EXPR   Initialize and load only if EXPR evaluates to a non-nil value.
:commands  Define autoloads for commands that that will be defined by the
           package. This is useful if the package is being lazily loaded.
:noinstall If noinstall is t, not run (straight-use-package NAME)."
  (declare (indent 1))
  (unless (memq :disabled args)
    (let ((-if (or (plist-get args :if) t))
          (-commands (plist-get args :commands))
          (-noinstall (plist-get args :noinstall)))
      `(when ,-if
         ,(unless -noinstall
            `(straight-use-package ,name))
         ,(when -commands
            `(let ((name-string ,(symbol-name (let ((a (cadr name)))
                                              (if (listp a) (car a) a)))))
                 ,@(mapcar (lambda (cmd)
                             `(unless (fboundp ',cmd) (autoload ',cmd name-string)))
                           (if (listp -commands) -commands (list -commands)))))))))

(straight-initialize-packages)

;; use package
(eval-when-compile (require 'use-package))

(provide 'core-straight)

;;; core-straight.el ends here
