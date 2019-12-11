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

(defcustom lib-pm-directory
  (file-truename (concat user-emacs-directory "libpm/"))
  "The directory where LIB-PM downloads packages to."
  :type 'directory)

(defvar lib-pm-recipe-alist ()
  "Contains the recopies for each package.
This is an alist of form:((package . properites)).
package is a symbol, properties is a plist.
Available keywords: :type, :host, repo, :dependency.

:type is a symobl representing the source, available options are 'git, 'url.
If none specified, default to 'git. TODO: 'url.

:host is a symobl representing the source, available options are 'github,
'gitlab. Just only use when :type is 'git.
If none specified, default to 'github.

:repo is string representing a repository from github or gitlab,
it sould be like \"user/repo\".
:dependency is a list of symbols of packages thar this package depends on.")

(defun lib-pm-install (package)
  "Install a PACKAGE."
  (lib-pm-handle-error
   (lib-pm-with-recipe (package recipe package-symbol)
     (if recipe
         (if (lib-pm-installed-p package)
             t
           (funcall 'lib-pm--git-install package-symbol recipe)
           (add-to-list 'load-path (concat (file-name-as-directory lpm-package-dir)
                                           (symbol-name package-symbol)))))
       nil)))

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
  (unless (file-exists-p lib-pm-directory)
    (make-directory lib-pm-directory t))
  (lpm--with-recipe (package recipe package-symbol)
    (member (symbol-name package-symbol) (directory-files lib-pm-directory))))

(defvar lib-pm-process-buffer " *pm-process*"
  "It is used for LIB-PM-PROCESS-BUFFER.")

(defun lib-pm--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir)
        (buffer lib-pm-process-buffer)
        status)
    (setq status (apply #'call-process command nil buffer t args))
    (if (eq status 0)
        nil
      (pop-to-buffer buffer)
      (error "%s fails." command))))

;;
;;; git install
(defun lib-pm--git-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let* ((host (plist-get recipe :host))
         (package-name (symbol-name package))
         (url-header (if (eq 'gitlab host)
                         "https://gitlab.com/"
                       "https://github.com/")))
    (let ((progress-reporter (make-progress-reporter
                              (format "Installing `%s' package..." package-name))))
      (lib-pm--command "git" lib-pm-directory "clone" "--depth"
                       "1"
                       (let ((repo (plist-get recipe :repo)))
                         (if repo
                             (concat url-header repo ".git")))
                       package-name)
      (progress-reporter-done progress-reporter))))

;;
;;; Initialize

(defmacro lib-pm-add-load-path ()
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (declare (indent defun))
  `(let ((dirs (cl-remove-if-not
                (lambda (dir) (file-directory-p dir))
                (directory-files ,lib-pm-directory t "^[^\\.]"))))
     (setq load-path (append (if dirs dirs (list ,lib-pm-directory)) load-path))))

(provide 'lib-pm)

;;; lib-pm.el ends here
