;;; lib-pm.el --- a simple package-manger -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: package-manager
;; Last-Updated: 2019-12-11 19:49:10


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

;; A simple Package-manger

;;; Change log:
;;
;; 12/11/19

;;; Code:

(require 'lib-f)
(require 'subr-x)

(defcustom lib-pm-directory (lib-f-join user-emacs-directory "libpms")
  "The directory where LIB-PM downloads packages to."
  :type 'directory)

(defcustom lib-pm-cache-file (lib-f-join lib-pm-directory "cache-loadfs.el")
  "save installed package information."
  :type 'file)

(defvar lib-pm--save-package-alist '() "save all installed package-alist.")
(defvar lib-pm--initialize-forcep nil)
(defun lib-pm-install (package-alist)
  "the PACKAGE is an alist of form:(PKG-NAME . RECIPES).
PKG-NAME is a symbol, RECIPES is a plist.
Abailable Keywords: fetcher, repo, dependency, pseudo. load-path.


:fetcher is a symobl representing the source, available options are 'gitub,'gitlab
and 'url. If none specified, default to 'gitub. TODO: 'url.

:repo is string representing a repository from github or gitlab,
it sould be like \"user/repo\".

:dependency is a list of symbols of packages thar this package depends on.

:pseudo is for pseudo packages. for example, ivy, cunsel & swiper are in one package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash
or dont.

:load-path is added somepath to `load-path'. for example pdf-tools, *.el is exists
in lisp/* dir."
  (lib-pm-handle-error
   (let ((package-name (car package-alist))
         (recipe (cdr package-alist)))
     (unless (lib-pm-installed-p package-name)
       (lib-f-make-dir lib-pm-directory)
       (funcall (intern (format "lib-pm--%s-install"
                                (symbol-name (or (plist-get recipe :fetcher)
                                                 'github))))
                package-name recipe)
       (cl-pushnew package-alist lib-pm--save-package-alist :test #'eqaul)
       (cl-pushnew (concat (file-name-as-directory lib-pm-directory)
                           (symbol-name package-name)
                           (when-let ((path (plist-get recipe :load-path)))
                             "/" path))
                   load-path :test #'string=)
       (lib-pm-initialize-package t)))
   (when (or lib-pm--initialize-forcep (file-exists-p lib-pm-cache-file))
     (setq lib-pm--initialize-forcep t)
     (lib-pm-initialize-package t))))

(defvar lib-pm-error-func (lambda (err) (message (error-message-string err)))
  "The default error handling function used by `lib--handle-error'.")

(defmacro lib-pm-handle-error (&rest form)
  "Return t if success, nil if fail.

Eval FORM. Handle error with `lib-pm-error-func'."
  `(condition-case err (progn ,@form t)
     ((error) (funcall lib-pm-error-func err)
      nil)))
;;
;;; installed command
(defun lib-pm--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process command nil t nil args))
        (error (buffer-string))))))

;; github install
(defun lib-pm--github-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let* ((package-name (symbol-name package))
         (url-header "https://github.com/")
         (progress-reporter (make-progress-reporter
                            (format "Installing `%s' package..." package-name))))
    (lib-pm--command "git" lib-pm-directory "clone" "--depth=1"
                     (when-let ((repo (plist-get recipe :repo)))
                       (concat url-header repo ".git"))
                     package-name)
    (progress-reporter-done progress-reporter)))

;; gitlab install
(defun lib-pm--gitlab-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let* ((package-name (symbol-name package))
         (url-header "https://gitlab.com/")
         (progress-reporter (make-progress-reporter
                             (format "Installing `%s' package..." package-name))))
    (lib-pm--command "git" lib-pm-directory "clone" "--depth=1"
                     (when-let ((repo (plist-get recipe :repo)))
                       (concat url-header repo ".git"))
                     package-name)
    (progress-reporter-done progress-reporter)))

;;
;;; the package is installed or not.
(defvar lib-pm--package-installed-list '() "save all package name.")
(defun lib-pm-get-package-installed-list ()
  "get all instelled package-name."
  (let ((package-list (mapcar #'car lib-pm--save-package-alist))
        (recipe-list (mapcar #'cdr lib-pm--save-package-alist)))
    (mapcar (lambda (recipe)
            (when-let ((pesudo (plist-get recipe :pesudo)))
              (if (listp pesudo)
                  (dolist (p pesudo)
                    (cl-pushnew p package-list))
                (cl-pushnew pesudo package-list))))
          recipe-list)
    (setq lib-pm--package-installed-list package-list)))

(defun lib-pm-installed-p (package)
  "Retrun t if PACKAGE is installed, nil if not."
  (let* ((package (if (listp package) (cdr package) package)))
    (if (and lib-pm--package-installed-list
             (not lib-pm--initalize-forcep))
        (memq package lib-pm--package-installed-list)
      (member (symbol-name package) (lib-f-list-directory lib-pm-directory)))))

;;
;;; the package load path
(defvar lib-pm--package-load-path '() "Save all installed package path.")
(defun lib-pm--set-load-path ()
  "add all package path."
  (setq lib-pm--package-load-path
        (mapcar (lambda (alist)
                  (let ((pkg (car alist))
                        (recipe (cdr alist)))
                    (concat (file-name-as-directory lib-pm-directory)
                            (symbol-name pkg)
                            (when-let ((path (plist-get recipe :load-path)))
                              "/" path))))
                lib-pm--save-package-alist)))

;;
;;; initialize
(defun lib-pm-initialize-package (&optional forcep)
  (let ((generated-autoload-file (file-name-sans-extension
                                  (file-name-nondirectory lib-pm-cache-file))))
    (when (or forcep (not (file-exists-p lib-pm-cache-file)))
      (when lib-pm--save-package-alist
        (lib-pm--set-load-path)
        (lib-pm-get-package-installed-list))
      (with-temp-file lib-pm-cache-file
        (dolist (path lib-pm--package-load-path)
          (let ((default-directory path))
            (require 'autoload)
            (dolist (f (lib-f-directory-el-files path))
              (let ((generated-autoload-load-name (file-name-sans-extension f)))
                (autoload-generate-file-autoloads f (current-buffer))))))
        (prin1 `(progn
                  (setq lib-pm--package-load-path ,lib-pm--package-load-path
                        lib-pm--save-package-alist ,lib-pm--save-package-alist
                        lib-pm--package-installed-list ,lib-pm--package-installed-list))
               (current-buffer))))))

(defun lib-pm-initialize ()
  (if (file-exists-p lib-pm-cache-file)
      (load lib-pm-cache-file :no-error :no-message)
    (mapcar (lambda (path)
              (push path load-path))
            lib-pm--package-load-path))
  (setq lib-pm--initalize-forcep t))

(provide 'lib-pm)

;;; lib-pm.el ends here
