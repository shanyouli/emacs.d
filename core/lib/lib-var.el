;;; lib-var.el --- some useful list functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: list

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

;; Some list-processing functions used

;;; Code:

;;
;;; clist plist alist 相互转换.
;; see@http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-1-8-4
(defun lib-var-clist-to-alist (clist)
  "CLIST --> alist."
  (if (null clist)
      '()
    (let ((hd (car clist))
          (tl (cdr clist)))
      (cons (list (car hd) (cdr hd))
            (lib-var-clist-to-alist tl)))))

(defun lib-var-alist-to-clist (alist)
  "ALIST --> clist."
  (if (null alist)
      '()
    (let ((hd (car alist))
          (tl (cdr alist)))
      (cons (cons (car hd) (cadr hd))
            (lib-var-alist-to-clist tl)))))

(defun lib-var-plist-to-alist (plist)
  "plist --> alist."
  (if (null plist)
      '()
    (cons (list (car plist) (cadr plist))
          (lib-var-plist-to-alist (cddr plist)))))

(defun lib-var-alist-to-plist (alist)
  "ALIST --> plist."
  (if (null alist)
      '()
    (let ((hd (car alist))
          (tl (cdr alist)))
      (cons (car hd) (cons (cadr hd) (lib-var-alist-to-plist tl))))))

(defun lib-var-plist-to-clist (plist)
  "PLIST --> ALIST."
  (if (null plist)
      '()
    (cons (cons (car plist) (cadr plist))
          (lib-var-plist-to-clist (cddr plist)))))

(defun lib-var-clist-to-plist (clist)
  "CLIST --> plist."
  (if (null clist)
      '()
    (let ((hd (car clist))
          (tl (cdr clist)))
      (cons (car hd) (cons (cdr hd) (lib-var-clist-to-plist tl))))))



;;
;;; 分离 plist 的 Key 和 values
(defun lib-var-plist-to-kv (plist)
  "Separates a property list into two lists of Keys and Values."
  (let ((clist (lib-var-plist-to-clist plist)))
    (cons (mapcar #'car clist) (mapcar #'cdr clist))))

;;
;;; 删除 plist 的 key 和 其对应的 values
(defun lib-var-delete-a-element-plist (key plist)
  "Delete KEY and VALUES in PLIST."
  (if (memq key plist)
      (let (q)
        (while plist
          (let ((a (car plist)))
            (when (and a (not (equal a key)))
              (setq q (plist-put q a (nth 1 plist))))
            (setq plist (cddr plist))))
        q)
      plist))

(defun lib-var-delete-same-element-in-list (lists)
  "Delete the same elemnet in a list."
  (if (null lists)
      '()
    (let ((elem (car lists)))
      (cons elem
            (lib-var-delete-same-element-in-list (delete elem lists))))))

(defun lib-var-list-to-str (lists &optional sper)
  "LISTS --> string."
  (mapconcat 'identity lists (or sper " ")))

(defun lib-var-delete-same-element-in-string (str &optional sepr)
  "Delete same element in a string."
  (let* ((strtolist (split-string str sepr)))
    (lib-var-list-to-str (lib-var-delete-same-element-in-list strtolist) sepr)))

(defun lib-var-list-eql (list1 list2)
  "Compare the two lists are the same."
  (if (null list1)
      (if list2 nil t)
    (let ((e (car list1))
          result)
      (cond ((null list2) (setq result nil))
            ((member e list2)
             (setq result t
                   list2 (delete e list2)))
            (t (setq result nil)))
      (if result (lib-var-list-eql (cdr list1) list2) nil))))

(provide 'lib-var)
;;; lib-list.el ends here
