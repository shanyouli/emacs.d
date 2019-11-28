;;; modules-key.el --- Function to bind key module -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: keybindings


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

;; Key bindings function.
;; Copy from @https://github.com/manateelazycat/lazy-load

;;; Change log:
;;
;; 2019/11/28:
;;        * rename `md-key/set+' to `md-key/set'
;;        * delete `md-key/unset+'
;;        * rename `md-key/unset-local+' to `md-key/unset-local'
;;        * delete `md-key/unset-global+', and `md-key/unset-global' macro
;;        * rename `md-key/set-local' to `md-key/set-local+'
;;        * rename `md-key/set-global' to `md-key/set-global+'
;;        * rename `md-key/set-ext' to `md-key/set-local'. and add `md-key/set-global' macro
;; 2019/11/27:
;;        * adjust `md-key/unset+'
;;        * remove `mdk/set-key!' and `mdk/set-keys!'
;;        * add function `md-key/set-ext', `md-key/set-local' and `md-key/set-global'
;; 2019/11/19:
;;        * add `md-key/unset-global+'
;;        * add `md-key/unset-local+', remove `md-key/unset-keys+'
;; 2019/11/13
;;        * Adjusting mdk/unset-key and mdk/unset-keys
;; 2019/11/09
;;        * and option `md-key-prefix'.


;;; Code:

(defcustom md-key-prefix nil
  "Set the default shortcut prefix When using `md-key/set-ext' and its spread Function Set shortcut."
  :type 'string)

;;
;;; 绑定一个快捷按键
(defun md-key/set (keymap key def)
  "Binding `def' shortcut function is `key', the key-map is `keymap'."
  (cond ((stringp key)
         (setq key (read-kbd-macro (concat key))))
        ((vectorp key) (setq key key))
        (t (signal 'wrong-type-argument (list 'array key))))
  (define-key keymap key def))

;;
;;; 移除快捷键

;;;###autoload
(defun md-key/unset-local (keymap &rest keybind)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, `KEYBIND' is list contain key."
  (let* ((keybind (copy-tree keybind))
         (len (length keybind))
         (key-list))
    (cond ((and (= len 1) (listp (car keybind)))
           (setq key-list (car keybind)))
          (t
           (setq key-list keybind)))
    (dolist (k key-list)
      (md-key/set keymap k nil))))

;;;###autoload
(defmacro md-key/unset-global (&rest keybind)
  "`keybind' button is not binding. the key-map is `current-global-map'"
  `(md-key/unset-local (current-global-map) ,@keybind))



;;;###autoload
(defun md-key/set-local (key-str def keymap &optional key-prefix filename)
  "函数绑定快捷按键. 用法:
(md-key/set-local KEY-STR DEF &OPTIONAL KEYMAP KEY-PREFIX FILENAME)

`KEYSTR' 是需要绑定的函数 `DEF' 的快捷键. 可选项 `KEYMAP' 是按键对应的 KEYMAP,
如果 KEYMAP 为 nil, 则 KEYMAP 为 全局的 map.
`KEY-PREFIX' 为当 KEY-STR 类型为 STRING 时的前缀.
`filename' 为当对应函数为被加载时, 确定函数对应的文件,并加载它."
  (let* ((key-prefix (cond ((and md-key-prefix key-prefix (booleanp key-prefix))
                            (concat md-key-prefix " "))
                           ((and key-prefix (stringp key-prefix))
                            (concat key-prefix " "))
                           (t nil)))
         (key (if key-prefix (concat key-prefix key-str) key-str)))
    (md-key/set keymap key def))
  (and filename (autoload def filename)))

;;;###autoload
(defmacro md-key/set-global (key-str def &optional key-prefix filename)
  `(md-key/set-local ,key-str ,def (current-global-map) ,key-prefix ,filename))

;;;###autoload
(defun md-key/set-local+ (key-alist keymap &rest rest)
  "对 `md-key/set-ext' 函数的扩展. 用法:
(md-key/set-local KEY-ALIST KEYMAP &rest REST)

`KEY-ALIST' 是对应函数 `md-key/set-ext' 中的 key-str 和 def
`REST' 一般包含 `md-key/set-ext' 中的 KEY-PREFIX 或 FILENAME 中的一个."
  (let ((rest (copy-tree rest))
        (len (length rest))
        (key-prefix)
        (filename))

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
        (md-key/set-local key def keymap key-prefix filename)))))

;;;###autoload
(defmacro md-key/set-global+ (key-alist &rest rest)
  "默认 KEYMAP 为 (current-global-map) 的 `md-key/set-local' 扩展函数."
  `(md-key/set-local+ ,key-alist (current-global-map) ,@rest))

(provide 'modules-key)

;;; modules-key.el ends here
