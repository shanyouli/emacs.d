;;; onekey-ext.el --- one-key Extensions -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/onekey
;; Keywords: key
;; Last-Updated: 2019-11-18 16:40:20


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

;; One-Key Extensions

;;; Change log:
;;
;; 11/18/19
;;        * 更改顺序 `menu-list'
;;        * Initialize
;;; Code:


(defun one-key--make-defun (cmd-name cmd-doc name menu-list &optional recursion-p)
  "Create a `one-menu-*' functions."
  `(defun ,cmd-name ()
     ,cmd-doc
     (interactive)
     (require 'one-key)
     (one-key-menu ,name ,menu-list t ,recursion-p)))

;;;###autoload
(defmacro defonekey (name recursion-p &optional docstring &rest heads)
  "A similar macro and defhydra. "
  (declare (indent defun) (doc-string 3))
  (setq heads (copy-tree heads))
  (condition-case-unless-debug err
      (let* ((menu-list)
             (name-upcase (upcase (format "%S" name)))
             (def-name (intern (format "one-key-%S/menu" name)))
            (name-menu (intern (format "%S/menu" name)))
            (name-docstring (format "The `one-key' menu for %s" name-upcase)))
        (dolist (h heads)
          (let ((len (length h)))
            (cond ((< len 2)
                   (error "Each head should have at least two items: %S" h))
                  ((= len 2)
                   (let ((h-command (cadr h))
                         (h-list))
                     (setq h-list (cons (cons (car h) (symbol-name h-command))
                                        h-command))
                     (setq menu-list (cons h-list menu-list))))
                  (t
                   (setq menu-list (cons (cons (cons (car h) (caddr h))
                                               (cadr h))
                                         menu-list))))))
        (setq menu-list (nreverse menu-list))
        `(progn
           (set (defvar ,(intern (format "%S/menu" name)) nil)
                ',menu-list)
           ,(one-key--make-defun def-name
                                 name-docstring
                                 name-upcase
                                 name-menu
                                 recursion-p)))))

(provide 'onekey-ext)

;;; onekey-ext.el ends here
