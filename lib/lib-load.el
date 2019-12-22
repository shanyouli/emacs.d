;;; lib-load.el --- Load configurations -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (load)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: Load
;; Last-Updated: 2019-12-12 15:00:28


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

;; Load configurations

;;; Change log:
;;
;; 12/12/19

;;; Code:

(defvar lib-load--feature-enable-alist nil
  "Stores whether a particular feature is provided alread.")

(defvar lib-load-message-p nil
  "Whether display message when use `lib-load-relative'.
But (LIB-LOAD-RELATIVE FILE &REST ARGS), ARGS is exists,
LIB-LOAD-MESSAGE-P will not work.")

(defmacro lib-load-message-error! (&rest body)
  "Eval BODY and print error message if ang."
  `(condition-case err
       (progn ,@body)
     (error (message (format "Error occured:\n%s\n" (error-message-string err))))))

;;;###autoload
(defun lib-safe-load (file &rest args)
  "Load FILE and don't error out. ARGS is as same as in `load'."
  (lib-load-message-error!
   (if (and lib-load-message-p (not args))
       (load file nil t)
     (apply #'load file args))))

(defun lib-load-or-create (file &rest args)
  "Load FILE if file exists, otherwise create it. ARGS is as same as in `load'."
  (if (file-exists-p file)
      (apply #'lib-safe-load file args)
    (save-excursion
      (find-file file)
      (save-buffer)
      (kill-buffer))))

;;;###autoload
(defun lib-load-relative (feature &rest args)
  "Load FILE relative to user-emacs-directory. ARGS are applied to `load'."
  (let ((symbol (lib-load--feature-symbol feature)))
    (unless (memq symbol lib-load--feature-enable-alist)
      (let ((file (concat (symbol-name symbol) ".el")))
        (apply #'lib-load-or-create (expand-file-name file user-emacs-directory)
               args)
        (push symbol lib-load--feature-enable-alist)))))

(defun lib-load--feature-symbol (feature)
  "FEATURE can be a symbol, a string. Return symbol."
  (pcase feature
    ((pred symbolp) feature)
    ((pred stringp) (file-name-sans-extension feature))
    (_ (error "Cannot make into package symbol: %s." feature))))

;;;###autoload
(defun lib-load-add-load-path (path &optional subdirp)
  "add PATH to `load-path',
if SUBDIR is non-nil, the subdirectory of PATH will add to `load-path'"
  (if subdirp
      (mapc (lambda (subpath) (push subpath load-path))
            (lib-f-list-directory path t))
    (push path load-path)))

(provide 'lib-load)

;;; lib-load.el ends here
