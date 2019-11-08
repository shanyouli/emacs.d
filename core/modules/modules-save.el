;;; modules-save.el --- Auto-save buffers, base on your setting -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: auto-save


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

;; auto-save saves buffers when you need.
;; reference:
;;          https://github.com/bbatsov/super-save
;;          https://github.com/manateelazycat/auto-save

;;; Code:

(defgroup modules-save nil
  "Smart-saving of buffers."
  :group 'modules-save)

(defvar md-save-mode-map (make-sparse-keymap)
  "modulse-save mode's keymap.")

(defcustom md-save-triggers
  '(switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `md/save-command+'."
  :group 'modules-save
  :type '(repeat symbol))

(defcustom md-save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger `md/save-command+'"
  :group 'modules-save
  :type '(repeat symbol))

(defcustom md-save-auto-save-when-idle t
  "If the file is automatically saved as t, the time delay,
otherwise use a trigger to automatically save the file. "
  :group 'modules-save
  :type 'boolean)

(defcustom md-save-idle-duraion 2
  "The number of seconds Emacs has to be idle. before auto-saveing the current buffer."
  :group 'modules-save
  :type 'float)

(defcustom md-save-remote-files t
  "Save remote files when t, ignore them otherwise."
  :group 'modules-save
  :type 'boolean)

(defcustom md-save-silent-p nil
  "prompted to save information when t, ignore theme otherwise."
  :group 'modules-save
  :type 'boolean)

(defcustom md-save-exclude nil
  "A list of regexps for buffer-file-name excludede from modules-save.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'modules-save
  :type '(repeat (choice regexp)))

(defun md/save-include-p! (filename)
  "Return non-nil if FILENAME doesn't match any of the `md-save-execlude'."
  (let ((checks md-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))

(defun md/save-command+ ()
  "Save buffer command."
  (when (and (buffer-file-name) ; Buffer associate with a filename?
             (buffer-modified-p) ; Buffer is modifiable?
             (file-writable-p (buffer-file-name)) ; Buffer is writable?
             (if (file-remote-p (buffer-file-name)) ; Buffer is remote-file and You can save?
                 md-save-remote-files t)
             (md/save-include-p! (buffer-file-name)) ; buffer is exclude?
             (or (not (boundp 'yas--active-snippets)) ; Yassnippet is not active?
                 (not yas--active-snippets))
             (or (not (boundp 'company-candidates)) ; Company is not active?
                 (not company-candidates)))

    (if md-save-silent-p
        (with-temp-message
            (with-current-buffer " *Minibuf-0*" (buffer-string))
          (let ((inhibit-message t))
            (basic-save-buffer)))
      (basic-save-buffer))
    t))

(defun md/save-current-buffer-command+ ()
  "Save the current buffer if needed."
  (set-buffer (current-buffer))
  (when (and (md/save-command+) (not md-save-silent-p))
    (message "# Saved %s" (buffer-name))))

(defun md/save-all-buffers-command+ ()
  (let ((save-buffer-list))
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (md/save-command+)
          (push (buffer-name) save-buffer-list))
      (unless md-save-silent-p
        (cond
         ;; It's stupid tell user if noting to save.
         ((= (length save-buffer-list) 1)
          (message "# Saved %s" (car save-buffer-list)))
         ((> (length save-buffer-list) 1)
          (message "# Saved %d files: %s"
                   (length save-buffer-list)
                   (mapconcat 'identity save-buffer-list ", "))))))))

(defun md/save-current-buffer-command-advice+ (&rest _args)
  "A simple wrapper around `md/save-current-buffer-command+' that's advice-friendly."
  (md/save-current-buffer-command+))

(defun md/save-all-buffers-command-advice+ (&rest _args)
  "A simple wrapper around `md/save-all-buffers-command+' that's advice-friendly."
  (md/save-all-buffers-command+))

(defun md/save-current-buffer-advise-trigger-commands+ ()
  "Apply `md/save-current-buffer-command-advice+' advice to the commands listed in `md-save-triggers'"
  (mapc (lambda (command)
          (advice-remove command #'md/save-current-buffer-command-advice+))
        md-save-triggers))

(defun md/save-all-buffers-advise-trigger-commands+ ()
  "Apply modules-save advice to the commands listed in `md-save-triggers'"
  (mapc (lambda (command)
          (advice-remove command #'md/save-all-buffers-command-advice+))
        md-save-triggers))

(defun md/save-current-buffer-remove-advice-from-trigger-commands+ ()
  "Remove `md/save-current-buffer-command-advice+' advice to the commands listed in `md-save-triggers'"
  (mapc (lambda (command)
          (advice-remove command #'md/save-current-buffer-command-advice+))
        md-save-triggers))

(defun md/save-all-buffers-remove-advice-from-trigger-commands+ ()
  "Remove `md/save-all-buffers-command-advice+' advice to the commands listed in `md-save-triggers'"
  (mapc (lambda (command)
          (advice-remove command #'md/save-all-buffers-command-advice+))
        md-save-triggers))

(defvar md-save--save-idle-timer)
(defvar md-save--all-save-idle-timer)

(defun md/save-current-buffer-enable ()
  "If `md-save-auto-save-when-idle' is true, Initialize modules-save idle timer.
Otherwise, use a trigger to save current buffer."
  (if md-save-auto-save-when-idle
      (setq md-save--save-idle-timer
            (run-with-idle-timer md-save-idle-duraion t #'md/save-current-buffer-command+))
    (md/save-current-buffer-advise-trigger-commands+)
    (dolist (hook md-save-hook-triggers)
      (add-hook hook #'md/save-current-buffer-command+))))

(defun md/save-current-buffer-disable ()
  "Stop modules-save idle timer  if `md-save--save-idle-timer' is set."
  (if md-save--save-idle-timer
      (cancel-timer md-save--save-idle-timer)
    (md/save-current-buffer-remove-advice-from-trigger-commands+)
    (dolist (hook md-save-hook-triggers)
      (remove-hook hook #'md/save-current-buffer-command+))))

(defun md/save-all-buffers-enable ()
  "if `md-save-auto-save-when-idle' is true, Initialize modules-save idle timer.
Otherwise, use a trigger to save all buffer."
  (if md-save-auto-save-when-idle
      (setq md-save--all-save-idle-timer
            (run-with-idle-timer md-save-idle-duraion  t #'md/save-all-buffers-command+))
    (md/save-all-buffers-advise-trigger-commands+)
    (dolist (hook md-save-hook-triggers)
      (add-hook hook #'md/save-all-buffers-command+))))

(defun md/save-all-buffers-disable ()
  "Stop modules-save idle timer  if `md-save--all-save-idle-timer' is set."
  (if md-save--all-save-idle-timer
      (cancel-timer md-save--all-save-idle-timer)
    (md/save-all-buffers-remove-advice-from-trigger-commands+)
    (dolist (hook md-save-hook-triggers)
      (remove-hook hook #'md/save-all-buffers-command+))))

;;;###autoload
(define-minor-mode md-save-mode
  "A minor mode the saves your Current buffers when they lose focus."
  :lighter " modules-save"
  :keymap md-save-mode-map
  (if md-save-mode (md/save-current-buffer-enable)
   (md/save-current-buffer-disable)))

;;;###autoload
(define-minor-mode md-save-global-mode
  "A minor mode the saves your Current buffers when they lose focus."
  :lighter " modules-save"
  :keymap md-save-mode-map
  :global t
  (if md-save-global-mode
      (md/save-all-buffers-enable)
    (md/save-all-buffers-disable)))

(provide 'modules-save)

;;; modules-save.el ends here
