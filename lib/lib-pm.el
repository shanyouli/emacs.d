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

(defvar lib-pm-recipe-alist '()
  "Contains the recopies for each package.
This is an alist of form:((package . properites)).
package is a symbol, properties is a plist.
Available keywords: :fetcher, repo, :dependency.

:fetcher is a symobl representing the source, available options are 'gitub,'gitlab
and 'url. If none specified, default to 'gitub. TODO: 'url.

:repo is string representing a repository from github or gitlab,
it sould be like \"user/repo\".

:dependency is a list of symbols of packages thar this package depends on.

:pseudo is for pseudo packages. for example, ivy, cunsel & swiper are in one package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash
or dont.

:load-path is added somepath to `load-path'. for example pdf-tools, *.el is exists in lisp/* dir.")

(defun lib-pm-install (package)
  "Install a PACKAGE."
  (lib-pm-handle-error
   (lib-pm-with-recipe (package recipe package-symbol)
     (when recipe
       (if-let ((pseudo (plist-get recipe :pseudo)))
           (lib-pm-install pseudo)
         (unless (lib-pm-installed-p package)
           (funcall (intern (format "lib-pm--%s-install"
                                    (symbol-name (or (plist-get recipe :fetcher)
                                                     'github))))
                    package-symbol recipe)
           (add-to-list 'load-path (concat (file-name-as-directory lpm-package-dir)
                                           (symbol-name package-symbol)
                                           (plist-get recipe :load-path)))))))))

(defvar lib-pm-error-func (lambda (err) (message (error-message-string err)))
  "The default error handling function used by `lib--handle-error'.")

(defmacro lib-pm-handle-error (&rest form)
  "Return t if success, nil if fail.

Eval FORM. Handle error with `lib-pm-error-func'."
  `(condition-case err (progn ,@form t)
     ((error) (funcall lib-pm-error-func err)
      nil)))

(defun lib-pm--package-symbol (package)
  "PACKAGE can be a recipe, a symol or a dir. Return package symbol."
  (pcase package
    ((pred symbolp) package)
    ((pred stringp) (intern (file-name-base (directory-file-name package))))
    ((pred listp) (car package))
    (_ (error "Cannot make into package symbol: %s." package))))

(defmacro lib-pm-with-recipe (symbols &rest body)
  "Process package and evaluate BODY.
If PACKAGE is a symbol or list, treat as package, if it is a string,
treate as dir.
RECIPE and PACKAGE-SYMBOL is the symbol represents
the recipe and package symbol.
\(fn (PACKAGE RECIPE PACKAGE-SYMBOL) BODY...)."
  (declare (indent 1))
  (let ((package-sym (nth 0 symbols))
        (recipe-sym (nth 1 symbols))
        (package-symbol-sym (nth 2 symbols)))
    `(let* ((,package-symbol-sym (lib-pm--package-symbol ,package-sym))
            (,recipe-sym (if (listp ,package-sym)
                             (cdr ,package-sym)
                           (alist-get ,package-symbol-sym lib-pm-recipe-alist))))
       ,@body)))

(defun lib-pm-installed-p (package)
  "Return t if PACKAG (symbol, recipe, dir string) in installed, nil if not."
  (ignore package)
  (lib-f-make-dir lib-pm-directory)
  (lpm--with-recipe (package recipe package-symbol)
    (member (symbol-name package-symbol) (directory-files lib-pm-directory))))

;;
;;; installed command
(defvar lib-pm-process-buffer " *pm-process*" "It is used for LIB-PM-PROCESS-BUFFER.")
(defun lib-pm--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir)
        (buffer lib-pm-process-buffer)
        status)
    (setq status (apply #'call-process command nil buffer t args))
    (unless (eq status 0)
      (pop-to-buffer buffer)
      (error "%s fails." command))))

;; github install
(defun lib-pm--github-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let ((package-name (symbol-name package))
        (url-header "https://github.com/"))
    (let ((progress-reporter (make-progress-reporter
                              (format "Installing `%s' package..." package-name))))
      (lib-pm--command "git" lib-pm-directory "clone" "--depth"
                       "1"
                       (if-let ((repo (plist-get recipe :repo)))
                           (concat url-header repo ".git"))
                       package-name)
      (progress-reporter-done progress-reporter))))

;; gitlib install
(defun lib-pm--githlib-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let* ((package-name (symbol-name package))
         (url-header "https://gitlab.com/")
         (progress-reporter (make-progress-reporter
                             (format "Installing `%s' package..." package-name))))
    (lib-pm--command "git" lib-pm-directory "clone" "--depth"
                     "1"
                     (if-let ((repo (plist-get recipe :repo)))
                         (concat url-header repo ".git"))
                     package-name)
    (progress-reporter-done progress-reporter)))

;;
;;; Initialize
(defun lib-pm-add-load-path ()
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (when (file-exists-p lib-pm-directory)
    (push (lib-f-list-directory lib-pm-directory t) load-path)))

(provide 'lib-pm)

;;; lib-pm.el ends here
