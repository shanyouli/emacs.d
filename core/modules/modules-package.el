;;; modules-package.el --- Myself Package Manager -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: nil
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: package-manager
;; Last-Updated: 2019-11-30 19:22:20


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

;; Myself Package Manager

;;; Change log:
;; 11/30/19
;;        * add `package-install+'
;;
;; 11/30/19
;;        * initialize, copy-from:

;;; Code:
(require 'package)
(require 'lib-autoload)

(defcustom lpm-package-dir (file-truename (concat user-emacs-directory "lpm/"))
  "The directory where LPM downloads packages to."
  :type 'directory)

(defvar lpm-package-autoload-file (expand-file-name "lpm-loadfs.el" lpm-package-dir)
  "auoload file.")

(defvar lpm-recipe-alist ()
  "Contains the recopies for each package.
This is an alist of form:((package . properties)).
package is a symbol, properties is a plist.
Avaliable keywords: :type, :host, :repo, :dependency.
:type is a symbol representing the source, available options are 'git, 'url.
If none specified, default to 'git.

:host is a symbol representing the source, available options are 'github, 'gitlab.
Just only use when :type is 'git.
If none specified, default to 'github.

:repo is string representing a repository from github or gitlab,
it should be like \"user/repo\".

:dependency is a list of symbols of packages thar this package depends on.
:pseudo is for pseudo packages. for example, ivy, cunsel & swiper are in one package dir
and subdir under that into load-path, if the package needs to add subdirs that are deeper
to load-path, use this key to specify a relative path to package-dir. No preceeding slash
or dont.")

(defun lpm-install (package)
  (lpm--handle-error
   (lpm--with-recipe (package recipe package-symbol)
     (unless (lpm-installed-p package)
       (if recipe
           (if-let ((pseudo (plist-get recipe :pseudo)))
               (lpm-install pseudo)
             (funcall 'lpm--git-install package-symbol recipe)
             (add-to-list 'load-path
                          (concat
                           (file-name-as-directory lpm-package-dir)
                           (symbol-name package-symbol))))
         (package-install+ package-symbol)))
     (lib-autoload-create-and-update-file lpm-package-dir lpm-package-autoload-file))))

(defun lpm-installed-p (package)
  "Return t if PACKAGE (symbol, recipe, dir string) in installed, nil if not."
  (ignore package)
  (lpm--with-recipe (package recipe package-symbol)
    (if-let ((pseudo (plist-get recipe :pseudo)))
        (lpm-installed-p pseudo)
      (or (package-installed-p package-symbol)
          (member (symbol-name package-symbol) (directory-files lpm-package-dir))))))

(defmacro lpm-add-load-path ()
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (declare (indent defun))
  (require 'cl-seq)
  `(let ((dirs (cl-remove-if-not
                (lambda (dir) (file-directory-p dir))
                (directory-files ,lpm-package-dir t "^[^\\.]"))))
     (setq load-path (append (if dirs dirs (list ,lpm-package-dir)) load-path))))

(defmacro lpm-initialize ()
  "Lpm initialize."
  (lpm-add-load-path)
  (load lpm-package-autoload-file :no-error :no-message))

(defvar lpm--error-func (lambda (err) (message (error-message-string err)))
  "The default error handling function used by `lpm--handle-error'.")

(defmacro lpm--handle-error (&rest form)
  "Eval FORM. Handle error with `lpm--error-func'.

Return t if success, nil if fail."
  `(condition-case err (progn ,@form t)
     ((error) (funcall lpm--error-func err)
      nil)))

(defun lpm--package-symbol (package)
  "PACKAGE can be a recipe, a symbol or a dir. Return package symbol."
  (pcase package
    ((pred symbolp) package)
    ((pred stringp) (intern (file-name-base (directory-file-name package))))
    ((pred listp) (car package))
    (_ (error "Cannot make  into package symbol: %s" package))))

(defmacro lpm--with-recipe (symbols &rest body)
  "Process package and evaluate BODY.
If PACKAGE is a symbol or list, treat as package,
if it is a string, treate as dir.
RECIPE and PACKAGE-SYMBOL is the symbol represents
the recipe and package symbol.
\(fn (PACKAGE RECIPE RACKAGE-SYMBOL) BODY...)."
  (declare (indent 1))
  (let ((package-sym (nth 0 symbols))
        (recipe-sym (nth 1 symbols))
        (package-symbol-sym (nth 2 symbols)))
    `(let* ((,package-symbol-sym (lpm--package-symbol ,package-sym))
            (,recipe-sym (if (listp ,package-sym)
                             (cdr ,package-sym)
                           (alist-get ,package-symbol-sym lpm-recipe-alist))))
       ,@body)))

(defun lpm--command (command dir &rest args)
  "Call process with COMMAND and ARGS in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (if (eq 0 (apply #'call-process command nil t nil
                       args))
          nil
        (error (buffer-string))))))
;;
;;; git install

(defun lpm--git-install (package recipe)
  "Clone the package specified by RECIPE and name it PACKAGE (symbol)."
  (let* ((host (plist-get recipe :host))
         (url-header (if (eq 'gitlab host)
                         "https://gitlab.com/"
                       "https://github.com/")))
    (lpm--command "git" lpm-package-dir "clone" "--depth"
                  "1"
                  (let ((repo (plist-get recipe :repo)))
                    (if repo
                        (concat url-header repo ".git")))
                  (symbol-name package))))

;; package-install extension
(defun package-install+ (package &optional no-refresh)
  "Ask elpa to install given PACKAGE."
  (cond ((or (assoc package package-archive-contents) no-refresh)
         (package-install package))
        (t
         (package-refresh-contents)
         (package-install package t))))

(provide 'modules-package)

;;; modules-package.el ends here
