;;; lib-key.el ---define-key extensions              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:tools

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

;; define-key extensions
;; see @https://github.com/manateelazycat/lazy-load/blob/master/lazy-load.el
;; see @http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-6-1-2

;;; dependences

(require 'lib-var)

;;; Code:

(defvar lib-key-personal-keybindings nil
  "Save all bindings performed by `lib-key'.

Elements have the form ((KEY . [MAP]) CMD ORIGINAL-CMD)")

(defsubst lib-key::concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append thems."
  (apply #'append (delete nil (delete (list nil) elems))))
;;; 移除快捷按键.

;;;###autoload
(defun lib-key-unset (&rest args)
  "This function is to little type when unset key binding.
ARGS format is: (keymaps key1 key2...) or (key1 key2 ...).
`KEYMAPS' is add keymap for some binding, default is `current-global-map'."
  (let ((keymaps (car args)))
    (if (stringp keymaps)
        (setq keymaps (current-global-map))
      (setq args (cdr args)))
    (mapc (lambda (key)
            (cond ((stringp key) (setq key (read-kbd-macro key)))
                  ((vectorp key) nil)
                  (t (signal 'wrong-type-argument (list 'array key))))
            (define-key keymaps key nil))
          args)))

;;
;;; 为交互函数设定快捷按键.

;;;###autoload
(cl-defmacro lib-key-define (&rest plist &key prefix map autoload &allow-other-keys)
  "为一个可交互函数绑定一个快捷按键.
ARGS 可以存在的 key 有 prefix, keymaps, autoload.
:PREFIX         后接一个 string 的 key.
:KEYMAP or :map 后接一个 keymap 默认为 `(current-global-map)'.
:AUTOLOAD       后接一个文件名, 如 :autoload \"test\", 将被展开为 (autoload def \"test\").

ARGS 默认格式为 (k1 func1 k2 func2 k3 func3 .....)."
  (declare (indent defun))
  (let ((key-prefix (or (concat prefix " ") ""))
        ;; (keymap (or map `(current-global-map)))
        (autofile autoload)
        (key-def (copy-tree plist)))
    (dolist (key (list :prefix :map :autoload))
      (setq key-def (lib-var-delete-a-element-plist key key-def)))
    (setq key-def (lib-var-plist-to-alist key-def))
    (macroexp-progn
     (lib-key::concat
      (when autoload
        (cl-mapcan (lambda (fun) `((autoload ,(car fun) ,autoload)))
                   (mapcar #'cdr key-def)))
      (cl-mapcan (lambda (def-key)
                   (let ((key (let ((test-key (nth 0 def-key)))
                                (if (stringp test-key)
                                    (concat key-prefix test-key)
                                  test-key)))
                         (fun (nth 1 def-key)))
                     `((lib-key ,key ,fun ,map))))
                 key-def)))))

;;;###autoload
(defmacro lib-key (key-name command &optional keymap predicate)
  "Bind KEY-NAME to COMMAND in KEYMA(`global-map' if not passed).

KEY-NAME may be a vector or string.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap and not a quoted symbol.

If PREDICATE is non-nil, it is a form evaluated to determine when a key should
be bound. It must return non-nil in such cases.

FROM: https://github.com/jwiegley/use-package/blob/master/bind-key.el."
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (kdescvar (make-symbol "kdesc"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar (if (vectorp ,namevar) ,namevar
                       (read-kbd-macro ,namevar)))
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (quote ,keymap)))
            (,bindingvar (lookup-key (or ,keymap global-map) ,keyvar)))
       (let ((entry (assoc ,kdescvar lib-key-personal-keybindings))
             (details (list ,command (unless (numberp ,bindingvar)
                                       ,bindingvar))))
         (if entry
             (setcdr entry details)
           (add-to-list 'lib-key-personal-keybindings (cons ,kdescvar details)))
         ,(if predicate
              `(define-key (or ,keymap global-map) ,keyvar
                 '(menu-item "" nil :filter (lambda (&optional _)
                                              (when ,predicate
                                                ,command))))
            `(define-key (or ,keymap global-map) ,keyvar ,command))))))

(defun lib-key--map-apply (fun xss)
  (mapcar (lambda (xs) (apply fun xs)) xss))

(provide 'lib-key)

;;; lib-key.el ends here
