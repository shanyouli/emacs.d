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
    (if -if `(when ,-if ,@forms) (macroexp-progn forms))))

;; add-hook 扩展
(defmacro add-hook! (hook &rest args)
  "Custom hook with HOOK and ARGS no need lambda."
  (declare (indent defun))
  (let ((-if (plist-get+ args :if))
        (-local (plist-get+ args :local))
        (-defer (plist-get+ args :defer))
        (-append (plist-get+ args :append))
        (hooks (if (cdr-safe (cadr hook))
                   (cadr hook)
                 (list (cadr hook))))
        (funcs (let ((val (car args)))
                 (if (memq (car-safe val) '(quote function))
                     (if (cdr-safe (cadr val)) (cadr val)
                       (list (cadr val)))
                   (list `(lambda (&rest _) ,@args)))))
        forms)
    (dolist (fn funcs)
      (setq fn
            (if (numberp -defer)
                `(lambda (&rest _) (run-with-idle-timer ,-defer nil (function ,fn)))
              `(function ,fn)))
      (dolist (h hooks)
        (push `(add-hook ',h  ,fn ,-append ,-local) forms)))
    (if -if
        `(when ,-if ,@forms)
      (macroexp-progn forms))))

(defmacro add-hook-once (hook f &optional append local)
  "Like `add-hook', remove after call with HOOK F &OPTIONAL APPEND LOCAL."
  (let ((func (intern (format "lye/run-once-%s"
                              (cond ((symbolp f) (symbol-name f))
                                    (t (symbol-name (cadr f))))))))
    `(progn
       (defun ,func ()
         (remove-hook ,hook ',func ,local)
         (funcall ,f)
         ;; (fmakunbound ',func)
         )
       (add-hook ,hook ',func ,append ,local))))

;; 判断程序是否运行
(defun lye-is-running-p (cmd)
  (let ((out (call-process "pgrep" nil nil t "-x" cmd)))
    (if (eq 0  out) t nil)))

;; 判断字体是否安装好了。
(defun lye-font-installed-p (fontname)
  "Return t, THE FONTNAME font is installed."
  (let ((font (if (stringp fontname) (font-spec :family fontname) fontname)))
    (find-font font)))

(defun lye-try-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (message "Running lye hook: %s" hook)
  (condition-case err
      (funcall hook)
    ((debug error)
     (signal 'lye-hook-error (list hook err))))
  ;; Return nil so `run-hook-wrapped' won't short circuit
  nil)

(defvar lye-buffer--warnings nil
  "List of errors during startup.")

(defun lye-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
ARGS: format string arguments"
  (let ((msg (apply 'format msg args)))
    (message "(Lye-emacs) Warning: %s" msg)
    (when message-log-max
      (add-to-list 'lye-buffer--warnings msg 'append))))

(defun lye-set-custom-variable (variable value &optional no-save)
  "Setting a variable and saved to `custom-file'.
NO-SAVE is non-nil, not save to `custom-file'."
  (let ((result t))
    (unless (or no-save (file-writable-p (bound-and-true-p custom-file)))
      (if after-init-time
          (let ((save-silently inhibit-message))
            (setq result nil)
            (custom-save-variables variable value))
        (add-hook 'after-init-hook
                  (lambda () (custom-save-variables variable value)))))
    (if result (setq variable value))))

(provide 'core-libs)

;;; core-libs.el ends here
