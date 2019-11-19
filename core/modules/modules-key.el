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
;; 2019/11/19:
;;        * add `md-key/unset-global+'
;;        * add `md-key/unset-local+', remove `md-key/unset-keys+'
;; 2019/11/13
;;        * Adjusting mdk/unset-key and mdk/unset-keys
;; 2019/11/09
;;        * and option `md-key-prefix'.


;;; Code:

(defcustom md-key-prefix nil
  "Set the default shortcut prefix When using `mdk/set-key!' Or `mdk/set-keys!' Set shortcut."
  :type 'string)

(defun md-key/unset-key+ (key-string keymap)
  "The function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-STRING' is no longer required to set the shortcut key."
  (cond ((stringp key-string) (setq key-string (read-kbd-macro (concat key-string))))
        ((vectorp key-string) nil)
        (t (signal 'wrong-type-argument (list 'array key-string))))
  (define-key keymap key-string nil))

;;;###autoload
(defun md-key/unset-local+ (keymap &rest keybind)
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
      (md-key/unset-key+ k keymap))))

;;;###autoload
(defun md-key/unset-global+ (&rest keybind)
  "`keybind' button is not binding. the key-map is `current-global-map'"
  (md-key/unset-local+ (current-global-map) keybind))

;;;###autoload
(defun mdk/set-key! (key-str def &optional keymap key-prefix filename)
  (let* ((keymap (or keymap (current-global-map)))
         (key-prefix (cond ((and md-key-prefix key-prefix (booleanp key-prefix))
                            (concat md-key-prefix " "))
                           ((and key-prefix (stringp key-prefix))
                            (concat key-prefix " "))
                           (t "")))
         (key (cond ((stringp key-str) (read-kbd-macro (concat key-prefix key-str)))
                    ((vectorp key-str) key-str)
                    (t (signal 'wrong-type-argument (list 'array key-str))))))
    (define-key keymap key def)
    (if filename
        (autoload def filename))))

;;;###autoload
(defun mdk/set-keys! (key-alist &optional keymap key-prefix filename)
  (let (key def)
    (dolist (element key-alist)
      (setq key (car element)
            def (cdr element))
      (mdk/set-key! key def keymap key-prefix filename))))

(provide 'modules-key)

;;; modules-key.el ends here
