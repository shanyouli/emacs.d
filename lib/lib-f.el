;;; lib-f.el --- file, list etc. extensions          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:functions

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

;; File List etc.

;;; Code:

(require 'seq)

(defun lib-delete-same-element-in-list (list)
  "Delete the same element in a list."
  (let ((old-list list)
        new-list)
    (while old-list
      (let ((element (car old-list)))
        (unless (member element new-list)
          (setq new-list (cons element new-list))))
      (setq old-list (cdr old-list)))
    (nreverse new-list)))

(defun lib-delete-same-element-in-string (string &optional separator)
  "Delete the same element in a STRING,"
  (let* ((sepr (or separator ":"))
         (strtolist (split-string string sepr))
         result)
    (setq result (lib-delete-same-element-in-list strtolist))
    (mapconcat 'identity result sepr)))

(defun lib-f-list-directory (dir &optional absolute)
  "Return a list of directories in DIR. Return absolute path if ABSOLUTE is t."
  ;; FULL argument in `directory-files' must be t,
  ;; otherwise 'file-directory-p' doesn't work.
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (and (not (string-match "/\\.\\{1,2\\}$" file))
                             (file-directory-p file)))
                      (directory-files dir t))))

(defun lib-f-list-subdirectory (dir)
  "Return a list of absolute directory and subdir in DIR."
  (let ((subdir (lib-f-list-directory dir t)))
    (nconc subdir (mapcan (lambda (dir) (lib-f-list-directory dir t)) subdir))))

(defun lib-f-directory-files (dir &optional absolute)
  "Return a list of directories in DIR. Return absolute path if ABSOLUTE is t."
  ;; FULL argument in `directory-files' must be t,
  ;; otherwise 'file-directory-p' doesn't work.
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (and (not (string-match "/\\.\\{1,2\\}$" file))
                             (file-regular-p file)))
                      (directory-files dir t))))

(defun lib-f-list-file-or-dir (dir &optional absolute)
  "Retrun a list of directories or file in DIR. Return absolute path if ABSOLUTE is t."
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (not (string-match "/\\.\\{1,2\\}$" file)))
                      (directory-files dir t))))

(defun lib-f-directory-el-files (dir &optional absolute)
  "Return a list of `*.el' in DIR. Return absolute path if ABSOLUTE is t."
  (mapcar (lambda (path)
            (if absolute
                path
              (file-name-nondirectory path)))
          (seq-filter (lambda (file)
                        (and (not (string-match "/\\.\\{1,2\\}$" file))
                             (file-regular-p file)
                             (string= (file-name-extension file) "el")))
                      (directory-files dir t))))

(defun lib-f-list-subfile (dir)
  "Return a list of absolute directory and subfiles in DIR."
  (let ((subdir (lib-f-list-file-or-dir dir t)))
    (nconc (seq-filter (lambda (file)
                         (and (file-regular-p file)
                              (string= (file-name-extension file) "el")))
                       subdir)
           (mapcan (lambda (dir)
                     (and (file-directory-p dir) (lib-f-directory-el-files dir t)))
                   subdir))))

(defun lib-f-join (&rest path-list)
  "Join paths in PATH-LIST."
  (if (eql (length path-list) 1)
      (car path-list)
    (expand-file-name (car (last path-list))
                      (apply #'lib-f-join (butlast path-list)))))

(defun lib-f-make-dir (dir)
  "If the DIR isn't exists, create it."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun lib-f-make-parent-dir (dir)
  "If the parent-dir of DIR isn't exists, create it."
  (let ((parent-dir (file-name-directory dir)))
    (lib-f-make-dir parent-dir)))

(provide 'lib-f)
;;; lib-f.el ends here
