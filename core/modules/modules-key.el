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

;;; Code:

;;;###autoload
(defun mdk/unset-key! (key-string &optional keymap)
  "The function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-STRING' is no longer required to set the shortcut key."
  (or keymap (setq keymap (current-global-map)))
  (cond ((stringp key-string) (setq key-string (read-kbd-macro (concat key-string))))
        ((vectorp key-string) nil)
        (t (signal 'wrong-type-argument (list 'array key-string))))
  (define-key keymap key-string nil))

;;;###autoload
(defun mdk/unset-keys! (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let ((keymap (or keymap (current-global-map))))
    (dolist (key key-list)
      (mdk/unset-key! key keymap))))

;;;###autoload
(defun mdk/set-key! (key-str def &optional keymap key-prefix filename)
  (let* ((keymap (or keymap (current-global-map)))
         (key-prefix (if key-prefix
                         (concat key-prefix " ")
                       ""))
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
