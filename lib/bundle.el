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

(defvar bundle--active-list '())

(defvar bundle-directories (list (lib-f-join lye-emacs-dir "bundles/")))

(defun display-all-bundles ()
  (mapcan (lambda (dir)
            (mapcar #'intern (lib-f-list-directory dir)))
          bundle-directories))

(defun bundle-active-p (bundle)
  (member bundle bundle--active-list))

(defun bundle-get-path (bundle)
  "Get the path of BUNDLE."
  (cl-some (lambda (parent)
             (let ((path (concat (file-name-directory parent) bundle)))
               (when (file-directory-p path) (file-name-as-directory path))))
           bundle-directories))

;;;###autoload
(defmacro bundle! (bundle &rest args)
  "Load a bundle.

Usage:

  (bundle! bundle-name
     [:keyword [option]])

:if EXPR   Initialize and load only if EXPR evaluates to a non-bil value.
:defer x   If X is t, Will run BUNDLE config after 0.1s.
           If the type of X is number, Will run BUNDLE Config after X seconds.
           If it is nil, Will run immediately.
:disabled  Don't run when t.
:commands  Run config.el when call COMMAND."
  (declare (indent 1))
  (unless (memq :disabled args)
    (let* ((-name (symbol-name bundle))
           (-dir (bundle-get-path -name))
           (-package (concat -dir "package"))
           (-config (concat -dir "config"))
           (-defer (let ((x (plist-get args :defer)))
                    (if x (if (numberp x) x 0.1) nil)))
           (-if (or (plist-get args :if) t))
           (-commands (plist-get args :commands)))
      `(when (and (not (bundle-active-p ',bundle))
                  ,-if)
         (cl-pushnew ',bundle bundle--active-list)
         (load ,-package t t)
         ,(bundle-config--command -commands -defer -config)))))

(defun bundle-config--defer (time file)
  (if time
      `(run-with-idle-timer ,time nil (lambda () (load ,file t t)))
    `(load ,file t t)))

(defun bundle-config--command (command time file)
  (if command
      `(let ((absolute-file-path ,(concat file ".el")))
         ,@(mapcar (lambda (cmd) `(autoload ',cmd absolute-file-path))
                (if (listp command)
                    command
                  (list command))))
    (bundle-config--defer time file)))



(provide 'bundle)
;;; bundle.el ends here
