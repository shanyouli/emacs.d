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
(require 'rx)

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



;; 列出特定的文件和文件夹
(defun lib-f--seq-filter (target &optional select hidden file-ext)
  "DIR 可以取 nil, dir, file. `nil' 表示选择文件和文件夹. `dir' 只取文件夹.
`file' 只取文件.
HIDDEN 为 t 时, 不显示隐藏文件.
FILE-EXIT type is string, 只取对应的文件扩展名的文件."
  (let ((select (cond ((eq select 'dir) (file-directory-p target))
                      ((eq select 'file) (file-regular-p target))
                      (t t)))
        (hidden (if hidden
                    (string-match (rx "/" (not (any "."))
                                      (zero-or-more (not (any "/"))) eos)
                                  target)
                  t))
        (file-ext (if file-ext
                      (string-match (eval `(rx "." ,file-ext eos))
                                    target)
                    t)))
    (and (not (string-match (rx "/" (** 1 2 ".") eol) target))
         select
         hidden
         file-ext)))

(defun lib-f-list-directory (dir &optional absolute)
  "Return a list of directories in DIR. Return absolute path if ABSOLUTE is t."
  ;; FULL argument in `directory-files' must be t,
  ;; otherwise 'file-directory-p' doesn't work.
  (mapcar (lambda (path)
            (if absolute path (file-name-nondirectory path)))
          (seq-filter (lambda (file) (lib-f--seq-filter file 'dir))
                      (directory-files dir t))))

(defun lib-f-list-subdirectory (dir)
  "Return a list of absolute directory and subdir in DIR."
  (let ((subdir (lib-f-list-directory dir t)))
    (nconc subdir (mapcan (lambda (dir) (lib-f-list-directory dir t)) subdir))))

(defun lib-f-directory-files (dir &optional absolute)
  "Return a list of directories in DIR. Return absolute path if ABSOLUTE is t."
  ;; FULL argument in `directory-files' must be t,
  ;; otherwise 'file-directory-p' doesn't work.
  (mapcar (lambda (path) (if absolute path (file-name-nondirectory path)))
          (seq-filter (lambda (file) (lib-f--seq-filter file 'file))
                      (directory-files dir t))))

(defun lib-f-list-file-or-dir (dir &optional absolute)
  "Retrun a list of directories or file in DIR. Return absolute path if ABSOLUTE is t."
  (mapcar (lambda (path) (if absolute path (file-name-nondirectory path)))
          (seq-filter #'lib-f--seq-filter (directory-files dir t))))

(defun lib-f-directory-el-files (dir &optional absolute nonext)
  "Return a list of `*.el' in DIR. Return absolute path if ABSOLUTE is t.
IF NONEXT is t, Returns a list of the file does not contain an extension."
  (mapcar (lambda (path)
            (let ((path (if absolute path (file-name-nondirectory path))))
              (if nonext (file-name-sans-extension path) path)))
          (seq-filter (lambda (file) (lib-f--seq-filter file 'file t "el"))
                      (directory-files dir t))))

(defun lib-f-list-subfile (dir)
  "Return a list of absolute directory and subfiles in DIR."
  (let ((subdir (lib-f-list-file-or-dir dir t)))
    (nconc (seq-filter (lambda (file) (lib-f--seq-filter file 'file t "el")) subdir)
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
  (unless (file-exists-p dir) (make-directory dir t)))

(defun lib-f-make-parent-dir (dir)
  "If the parent-dir of DIR isn't exists, create it."
  (let ((parent-dir (file-name-directory dir)))
    (lib-f-make-dir parent-dir)))

(provide 'lib-f)
;;; lib-f.el ends here
