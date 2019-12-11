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

;;; Code:

;; 为一个交换函数绑定一个快捷键
(defun lib-key-set (keymap key def)
  "Binding DEF shortcut function is KEY, the Key-map is KEYMAP."
  (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
        ((vectorp key) (setq key key))
        (t (signal 'wrong-type-argument (list 'array key))))
  (define-key keymap key def))

;;
(defun lib-key-unset-local (keymap &rest keybind)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, `KEYBIND' is list contain key."
  (dolist (k keybind)
    (lib-key-set keymap k nil)))

(defmacro lib-key-unset-global (&rest keybind)
  "`keybind' button is not binding. the key-map is `current-global-map'"
  `(lib-key-unset-local (current-global-map) ,@keybind))

;;
(defvar lib-key-prefix nil "lib-key-set-* 设置按键的前缀.")

(defun lib-key-set-local (key-str def keymap &optional key-prefix filename)
  "`KEYSTR' 是需要绑定的函数 `DEF' 的快捷键. 可选项 `KEYMAP' 是按键对应的 KEYMAP,
如果 KEYMAP 为 nil, 则 KEYMAP 为 全局的 map.
`KEY-PREFIX' 为当 KEY-STR 类型为 STRING 时的前缀.
`filename' 为当对应函数为被加载时, 确定函数对应的文件,并加载它."
  (let* ((key-prefix (cond ((and lib-key-prefix key-prefix (booleanp key-prefix))
                            (concat lib-key-prefix " "))
                           ((and key-prefix (stringp key-prefix))
                            (concat key-prefix " "))
                           (t nil)))
         (key (if key-prefix (concat key-prefix key-str) key-str)))
    (lib-key-set keymap key def))
  (and filename (autoload def filename)))

(defmacro lib-key-set-global (key-str def &optional key-prefix filename)
  `(lib-key-set-local ,key-str ,def (current-global-map) ,key-prefix ,filename))

;;
(defun lib-key-set-locals (key-alist keymap &rest rest)
  "
`KEY-ALIST' 是对应函数 `lib-key-set-local' 中的 key-str 和 def
`REST' 一般包含 `lib-key-set-local' 中的 KEY-PREFIX 或 FILENAME 中的一个."
  (let ((rest (copy-tree rest))
        (len (length rest))
        key-prefix
        filename)

    (when rest
      (setq  key-prefix (plist-get rest :prefix)
             filename (plist-get rest :file))
      (unless (and key-prefix filename)
        (setq key-prefix (car rest)
              filename (if (= 2 len) (cadr rest) nil))))

    (let (key def)
      (dolist (element key-alist)
        (setq key (car element)
              def (cdr element))
        (lib-key-set-local key def keymap key-prefix filename)))))
(defmacro lib-key-set-globals (key-alist &rest rest)
  `(lib-key-set-locals ,key-alist (current-global-map) ,@rest))

(provide 'lib-key)

;;; lib-key.el ends here
