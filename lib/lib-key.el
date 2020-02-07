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
:AFTER
ARGS 默认格式为 (k1 func1 k2 func2 k3 func3 .....)."
  (declare (indent defun))
  (let ((key-prefix (or (concat prefix " ") ""))
        (keymap (or map `(current-global-map)))
        (autofile autoload)
        (key-def (copy-tree plist)))
    (dolist (key (list :prefix :map :autoload))
      (setq key-def (lib-var-delete-a-element-plist key key-def)))
    (setq key-def (lib-var-plist-to-alist key-def))
    `(progn
       ,@(lib-key--map-apply
          (lambda (key fun)
            (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
                  ((vectorp key) nil)
                  (t (signal 'wrong-type-argument (list 'array key))))
               `(define-key ,keymap ,key ,fun))
          key-def)
       (when ,autoload
         ,@(lib-key--map-apply
            (lambda (fun) `(autoload ,fun ,autoload))
            (mapcar #'cdr key-def))))))

(defun lib-key--map-apply (fun xss)
  (mapcar (lambda (xs) (apply fun xs)) xss))

(provide 'lib-key)

;;; lib-key.el ends here
