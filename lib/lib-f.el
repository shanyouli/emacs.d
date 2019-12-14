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

(defun lib-delete-same-element-in-list (list)
  "Delete the same element in a list."
  (let ((old-list list)
        new-list)
    (while old-list
      (let ((element (car old-list)))
        (when (and element (not (member element new-list)))
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

(provide 'lib-f)
;;; lib-f.el ends here
