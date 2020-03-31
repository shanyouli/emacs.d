;;; bundle.el --- Bundle-Framework Manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: framework

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

;; Bundle-Framework Manager

;;; Code:
(require 'subr-x)
(require 'cl-macs)
(require 'lib-key)

(defvar bundle--active-list '())

(defvar bundle-directories (list (lib-f-join lye-emacs-dir "bundles/")))

(defun display-all-bundles ()
  (mapcan (lambda (dir)
            (mapcar #'intern (lib-f-list-directory dir)))
          bundle-directories))

(defun bundle-active-p (bundle)
  (memq bundle bundle--active-list))

(defun bundle-exist-p (bundle)
  (memq bundle (display-all-bundles)))

(defun bundle-get-path (bundle)
  "Get the path of BUNDLE."
  (cl-some (lambda (parent)
             (let* ((bundle-name (if (stringp bundle) bundle (symbol-name bundle)))
                    (path (concat (file-name-directory parent) bundle-name)))
               (when (file-directory-p path) (file-name-as-directory path))))
           bundle-directories))

(defsubst bundle/concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append thems."
  (apply #'append (delete nil (delete (list nil) elems))))

(defun bundle-keys:package (bundle bundle-path)
  `((push ',bundle bundle--active-list)
    (load ,(concat bundle-path "package.el") t t)))

(defun bundle-keys:config (defer commands bundle-path)
  (let ((conf-path (concat bundle-path "config.el")))
    (cond
     (commands
      (cl-mapcan
       (lambda (cmd) `((autoload ',cmd ,conf-path nil t)))
       (if (listp commands) commands (list commands))))
     (defer
       (let ((time (if (numberp defer) defer 0.1)))
         `((run-with-idle-timer
            ,time nil
            (lambda (&rest _) (load ,conf-path t t))))))
     (t
      `((load ,conf-path t t))))))

(defun bundle-keys:menu (menu bundle-path)
  (when menu
    (let ((key-path (concat bundle-path "key.el")))
      (cl-mapcan
       (lambda (cmd) `((autoload ',cmd ,key-path nil t)))
       (if (listp menu) menu (list menu))))))

;;;###autoload
(cl-defmacro bundle!
    (bundle &key disabled if defer commands menu)
  "Load a bundle.

Usage:

  (bundle! bundle-name
     [:keyword [option]])

:if EXPR   Initialize and load only if EXPR evaluates to a non-bil value.
:defer x   If X is t, Will run BUNDLE config after 0.1s.
           If the type of X is number, Will run BUNDLE Config after X seconds.
           If it is nil, Will run immediately.
:disabled  Don't run when t.
:commands  Run config.el when call COMMAND.
:menu CMD   Delay load hydra menu."
  (declare (indent 1))
  (unless (or disabled (not (bundle-exist-p bundle)))
    (let ((bundle-dir (bundle-get-path bundle))
          (-if (or if t)))
      (macroexp-progn
       `((when (and ,-if (not (bundle-active-p ',bundle)))
           ,@(mapcar
              'identity
              (bundle/concat
               (bundle-keys:package bundle bundle-dir)
               (bundle-keys:config defer commands bundle-dir)
               (bundle-keys:menu menu bundle-dir)))))))))

;;;###autoload
(defmacro with-after-bundle (bundle &rest body)
  "When the bundle is started, executed body."
  (declare (indent 1) (debug t))
  `(eval-after-load
       ,(concat (bundle-get-path (if (cdr-safe bundle)
                                     (cadr bundle)
                                   bundle))
                "package.el")
     (lambda () ,@body)))

;;;###autoload
(defmacro bundle-key! (bundle &rest args)
  "When bundle is active. bind key."
  (declare (indent 4) (debug t))
  (let ((bundle (if (cdr-safe bundle)
                    (cadr bundle)
                  bundle)))
    `(if (bundle-active-p ',bundle)
         ,(macroexp-progn (lib-key--form args))
       (with-after-bundle ',bundle
         ,(macroexp-progn (lib-key--form args))))))

(provide 'bundle)
;;; bundle.el ends here
