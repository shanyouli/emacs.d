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
(declare-function which-key-add-key-based-replacements 'which-key)

;;; Code:

(defvar lib-key-personal-keybindings nil
  "Save all bindings performed by `lib-key'.

Elements have the form ((KEY . [MAP]) CMD ORIGINAL-CMD)")

(defsubst lib-key--list2alist (lists)
  "A list to alist, But The first element can not be a list."
  (if (null lists)
      '()
    (let ((hd (car lists)))
      (if (listp hd)
          (cons hd (lib-key--list2alist (cdr lists)))
        (cons (list hd (cadr lists))
              (lib-key--list2alist (cddr lists)))))))

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
(defmacro lib-key (key-name command &optional keymap predicate package)
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
        (bindingvar (make-symbol "binding"))
        (cmd (make-symbol "command"))
        forms)
    (setq forms
          `(let* ((,namevar ,key-name)
                  (,keyvar (if (vectorp ,namevar) ,namevar
                             (read-kbd-macro ,namevar)))
                  (,kdescvar (cons (if (stringp ,namevar) ,namevar
                                     (key-description ,namevar))
                                   (quote ,keymap)))
                  (,bindingvar (lookup-key (or ,keymap global-map) ,keyvar))
                  (,cmd ,(if predicate
                             `'(menu-item "" nil :filter (lambda (&optional _)
                                                           (when ,predicate
                                                             ,command)))
                           command)))
             (let ((entry (assoc ,kdescvar lib-key-personal-keybindings))
                   (details (list ,command (unless (numberp ,bindingvar)
                                             ,bindingvar))))
               (if entry
                   (setcdr entry details)
                 (add-to-list 'lib-key-personal-keybindings (cons ,kdescvar details)))
               (define-key (or ,keymap global-map) ,keyvar ,cmd))))
    (if package
        `(eval-after-load ',package ',forms)
      forms)))

;;;###autoload
(defmacro lib-keys (&rest args)
  "为一个可交互函数绑定一个快捷按键.
ARGS 可以存在的 key 有 prefix, keymaps, autoload. package

:PREFIX         后接一个 string 的 key.
:KEYMAP or :map 后接一个 keymap 默认为 `global-map' or (package map).
                PACKAGE 为 MA 所在文件名
:AUTOLOAD       后接一个文件名, 如 :autoload \"test\", 将被展开为 (autoload def \"test\").
:package        主要用于设置 keymap 且非 global-map 后, 快捷键在 keymap
                对应文件加载后生效.避免报错.
:doc DOCSTRING 添加文本说明.
ARGS 默认格式为 (k1 func1 k2 func2 k3 func3 .....)."
  (macroexp-progn (lib-key--form args)))

;;;###autoload
(cl-defmacro lib-key-definer (name &key doc prefix)
  "设置按键的前缀.
:prefix key         - 表示使用的前缀为 key,如果不存在,则使用 NAME 的值.
:doc docsting       - 对这个案件的文本说明."
  (declare (indent defun) (doc-string 3))
  (let ((prefix (or prefix (symbol-value name)))
        (dosctring (or doc (symbol-name name))))
    `(defmacro ,name (&rest args)
       ,dosctring
       (declare (indent defun))
       (macroexp-progn (lib-key--form args ,prefix)))))

(defun lib-key--form (args &optional prefix)
  "Bind multiple keys at once.
:map MAP          - a keymap into which the keybindings should be added
:prefix KEY       - when keybinding is a string. As a prefix key.
:package PACKAGE  - From PACKAGE function as.
:autoload FILE    - From PACKAGE function as.
:filter FORM      - optional for to determine when bindings apply"
  (let* (map doc pkg kv-list filter file)
    (setq kv-list (lib-key--list2alist args))
    (let ((map-alist (or (assoc :map kv-list) (assoc :keymap kv-list))))
      (when map-alist
        (setq map (cadr map-alist)
              kv-list (delete map-alist kv-list))
        (when (listp map)
          (setq pkg (car map)
                map (cdr map)))))
    (let ((file-alist (or (assoc :package kv-list )
                          (assoc :autoload kv-list))))
      (if file-alist
          (setq file (cadr file-alist)
                kv-list (delete file-alist kv-list))))
    (let ((prefix-alist (assoc :prefix kv-list)))
      (if prefix-alist
          (setq prefix (cadr prefix-alist)
                kv-list (delete prefix-alist kv-list))))
    (let ((filter-alist (assoc :filter kv-list)))
      (if filter-alist
          (setq filter (cadr filter-alist)
                kv-list (delete filter-alist kv-list))))
    (setq kv-list (delete (assoc :doc kv-list) kv-list))

    ;; key binding arguments
    (cl-flet
        ((bindkey-after-load-pkg
          (map bindings)
          (if (and map pkg (not (eq map 'global-map)))
              `((if (boundp ',map)
                    ,(macroexp-progn bindings)
                  (eval-after-load ,(if (symbolp pkg) `',pkg pkg)
                    ',(macroexp-progn bindings))))
            bindings)))
      (append
       (when file
         (cl-mapcan
          (lambda (def)
            `((autoload ,def ,(if (symbolp file) (symbol-name file) file) nil t)))
          (mapcar #'cadr kv-list)))
       (bindkey-after-load-pkg
        map
        (cl-mapcan (lambda (kv)
                     (let ((key (nth 0 kv))
                           (def (nth 1 kv))
                           (map (or (nth 2 kv) map))
                           (filter (or (nth 3 kv) filter))
                           (package (nth 4 kv)))
                       (if (and (stringp key) prefix)
                           (setq key (concat prefix " " key)))
                       (if (and map (not (eq map 'global-map)))
                           `((lib-key ,key ,def ,map ,filter ,package))
                         `((lib-key ,key ,def nil ,filter ,package)))))
                   kv-list))))))

(provide 'lib-key)

;;; lib-key.el ends here
