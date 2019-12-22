;;; lib-autoload.el --- Autoload extensions          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: autoload

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

;; Autoload extensions

;;; Code:

(require 'lib-f)
(require 'subr-x)

(defcustom lib-autoload-save-directory (lib-f-join user-emacs-directory "autoloads")
  "Autoload Save Directory."
  :type 'directory)



(defvar lib-autoload-directory-alist nil
  "Need to generate a list of all files stored autoload, the corresponding file.
The default format is '((dir . target))")

;;;###autoload
(defun lib-autoload-create-and-update-file (dir target &optional forcep save-to-alist-p)
  "Autoload directory DIR generated for a file named TARGET in.
If the FORCEP is non-nil, forcibly regenerate a new DIR file autoload it.
If th savep is non-nil, will run (push '(dir . target) lib-autoload-directory-alist)"
  (let ((generated-autoload-file (file-name-sans-extension
                                  (file-name-nondirectory target))))
    (when (or forcep (not (file-exists-p target)))
      (require 'autoload)
      (lib-f-make-parent-dir target)
      (with-temp-file target
        (dolist (f (lib-f-list-subfile dir))
          (let ((generated-autoload-load-name (file-name-sans-extension f)))
            (autoload-generate-file-autoloads f (current-buffer))))
        (when save-to-alist-p
          (prin1 `(push '(,dir . ,target) lib-autoload-directory-alist)
                 (current-buffer)))
        (insert (string-join `("\n"
                               ,(char-to-string ?\C-l)
                               ";; Local Varibles:"
                               ";; version-control: never"
                               ";; no-byte-compile: t"
                               ";; no-update-autoloads: t"
                               ";; coding: utf-8"
                               ";; End:"
                               ,(format ";;; %s ends here"
                                        (file-name-nondirectory target)))
                             "\n"))))
    (load target :no-error :no-message)))

(defun lib-autoload--get-true-file (fname)
  "Get true-file-path."
  (if (string= fname (file-name-nondirectory fname))
      (lib-f-join lib-autoload-save-directory
                  (if (file-name-extension fname)
                      fname
                    (concat fname "-loadfs.el")))
    (lib-f-make-parent-dir fname)
    fname))

(defun lib-autoload--generate-file (dir-target &optional forcep)
  (let ((dir (car dir-target))
        (target (lib-autoload--get-true-file (cdr dir-target))))
    (and (symbolp dir) (setq dir (symbol-value dir)))
    (lib-autoload-create-and-update-file dir target forcep t)))

;;;###autoload
(defun lib-autoload-generate-file-list (alist)
  (mapc #'lib-autoload--generate-file alist))

(defun lib-autoload/update-file (path)
  (interactive
   (list (intern
          (completing-read "Need generated-autoload PATH: "
                           (lib-delete-same-element-in-list
                            lib-autoload-directory-alist)))))
  (if (symbolp path)
      (setq path (symbol-name path)))
  (let ((dir-target (assoc path lib-autoload-directory-alist)))
    (lib-autoload--generate-file dir-target t))
  (setq lib-autoload-directory-alist
        (lib-delete-same-element-in-list lib-autoload-directory-alist)))

(defun lib-autoload/update-all-file ()
  (interactive)
  (mapc (lambda (dir-target) (lib-autoload--generate-file dir-target t))
        lib-autoload-directory-alist)
  (setq lib-autoload-directory-alist
        (lib-delete-same-element-in-list lib-autoload-directory-alist)))

(provide 'lib-autoload)
;;; lib-autoload.el ends here
