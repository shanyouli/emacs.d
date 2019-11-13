;;; core/autoload/one-key.el -*- lexical-binding: t -*-

(defun one-key--make-defun (cmd-name cmd-doc name menu-list &optional recursion-p)
  "Create a `one-menu-*' functions."
  `(defun ,cmd-name ()
     ,cmd-doc
     (interactive)
     (require 'one-key)
     (one-key-menu ,name ,menu-list  nil ,recursion-p)))

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
        `(progn
           (set (defvar ,(intern (format "%S/menu" name)) nil)
                ',menu-list)
           ,(one-key--make-defun def-name
                                 name-docstring
                                 name-upcase
                                 name-menu
                                 recursion-p)))))
