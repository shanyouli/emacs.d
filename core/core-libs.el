;;; core-libs.el --- core libs -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: lib
;; Last-Updated: 2019-11-28 17:38:05


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

;; core library

;;; Change log:
;;
;; 11/28/19
;;        * initialize
;;; Code:

(defun plist-get+ (args key &optional default)
  "Custom `plist-get' with ARGS and KEY DEFAULT."
  (or (plist-get args key)
      (plist-get (cdr args) key)
      default))

;; run-with-idle-timer 扩展
(defmacro run-with-idle-timer! (&rest args)
  "Delay operates without `lambda'."
  (declare (indent defun))
  (let ((-if (plist-get+ args :if t))
         (-secs (plist-get+ args :defer))
         (-repeats (plist-get+ args :repeat nil))
         (funcs (let ((val (car args)))
                 (if (memq (car-safe val) '(quote function))
                     (if (cdr-safe (cadr val)) (cadr val)
                       (list (cadr val)))
                   (list `(lambda (&rest _) ,@args)))))
        forms)
    (unless (numberp -secs)
      (setq -secs 0.5))
    (dolist (fn funcs)
      (push `(run-with-idle-timer ,-secs ,-repeats (function ,fn)) forms))
    `(when ,-if ,@forms)))

;; add-hook 扩展
(defmacro add-hook! (hook &rest args)
  "Custom hook with HOOK and ARGS no need lambda."
  (declare (indent defun))
  (let ((-if (plist-get+ args :if t))
        (-local (plist-get+ args :local))
        (-defer (plist-get+ args :defer))
        (-append (plist-get+ args :append))
        (funcs (let ((val (car args)))
                 (if (memq (car-safe val) '(quote function))
                     (if (cdr-safe (cadr val)) (cadr val)
                       (list (cadr val)))
                   (list `(lambda (&rest _) ,@args)))))
        forms)
    (dolist (fn funcs)
      (setq fn (if (numberp -defer)
                   `(lambda (&rest _)
                      (run-with-idle-timer ,-defer nil
                                           (function ,fn)))
                 `(function ,fn)))
      (push `(add-hook ,hook  ,fn ,-append ,-local) forms))
    `(when ,-if ,@forms)))

(defmacro add-hook-once (hook f &optional append local)
  "Like `add-hook', remove after call with HOOK F &OPTIONAL APPEND LOCAL."
  (let ((func (intern (format "lye/run-once-%s"
                              (cond ((symbolp f) (symbol-name f))
                                    (t (symbol-name (cadr f))))))))
    `(progn
       (defun ,func ()
         (remove-hook ,hook ',func ,local)
         (funcall ,f)
         (fmakunbound ',func))
       (add-hook ,hook ',func ,append ,local))))

(defmacro autoload! (fun name &rest args)
  (declare (indent 1))
  `(condition-case err
       (unless (fboundp ,fun)
         (apply #'autoload ,fun ,name ,args))
     (error (message (format "Error occured:\n%s\n" (error-message-string err))))))

;; Add after-load-theme-hook
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :after #'run-after-load-theme-hook)

;; 判断程序是否运行
(defun lye-is-running-p (cmd)
  (let ((out (call-process "pgrep" nil nil t "-x" cmd)))
    (if (eq 0  out) t nil)))

;; 判断字体是否安装好了。
(defun lye-font-installed-p (fontname)
  "Return t, THE FONTNAME font is installed."
  (let ((font (if (stringp fontname) (font-spec :family fontname) fontname)))
    (find-font font)))

(provide 'core-libs)

;;; core-libs.el ends here
