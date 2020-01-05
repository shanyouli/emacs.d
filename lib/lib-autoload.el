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
(require 'lib-var)
(require 'lib-load)

(autoload 'string-join "subr-x")

(defcustom lib-autoload-save-with-custom nil
  "If it is t, use custom-file save `lib-autoload--directory-alist'.
Otherwise, use *-loadfs.el save"
  :type 'boolean)

(defcustom lib-autoload-sans-extension-file (lib-f-join user-emacs-directory "core.pkg")
  "Save all generate autoload files."
  :type 'file)

(defcustom lib-autoload-initialize-list '()
  "Autoload files need to generate a list of file folders."
  :type 'list)



(defvar lib-autoload--initialized-list nil)

(defun lib-autoload--generate-update-file (lists target)
  (require 'autoload)
  (let ((generated-autoload-file (file-name-nondirectory target)))
    (with-temp-buffer
      (dolist (l lib-autoload-initialize-list)
        (setq l (pcase l
                  ((pred symbolp) (symbol-value l))
                  ((pred stringp) l)
                  (_ (error "Don't know the `%S' symbol." l))))
        (dolist (f (lib-f-list-subfile l))
          (let ((generated-autoload-load-name (file-name-sans-extension f)))
            (autoload-generate-file-autoloads f (current-buffer)))))
        (unless lib-autoload-save-with-custom
          (prin1 `(setq lib-autoload--initialized-list ',lib-autoload-initialize-list)
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
                             "\n"))
        (let ((fname (concat target ".el")))
          (write-file fname)
          (byte-compile-file fname))))
  (lib-safe-load target t t))

;;;###autoload
(defun lib-autoload-initialize (&optional forcep)
  (unless forcep
      (if lib-autoload-save-with-custom
      (unless (and lib-autoload-initialize-list
                   (lib-var-list-eql lib-autoload-initialize-list
                                     lib-autoload--initialized-list)
                   (lib-safe-load lib-autoload-sans-extension-file t t))
        (setq forcep t)
        (lib-autoload--save-loaded-dirs))
    (unless (and (file-exists-p (concat lib-autoload-sans-extension-file ".el"))
                 (lib-safe-load lib-autoload-sans-extension-file t t)
                 (lib-var-list-eql lib-autoload--initialized-list
                                   lib-autoload-initialize-list))
      (setq forcep t))))
  (when forcep
    (lib-autoload--generate-update-file lib-autoload-initialize-list
                                        lib-autoload-sans-extension-file)))

(defun lib-autoload--save-loaded-dirs ()
  "Set and save `lib-autoload--initialized-list'."
  (when lib-autoload-save-with-custom
    (if after-init-time
        (let ((save-silently inhibit-message))
          (customize-save-variable 'lib-autoload--initialized-list
                                   lib-autoload-initialize-list))
      (add-hook 'after-init-hook #'lib-autoload--save-loaded-dirs))))

(defun lib-autoload/update ()
  (interactive)
  (lib-autoload--generate-update-file lib-autoload-initialize-list
                                      lib-autoload-sans-extension-file)
  (lib-autoload--save-loaded-dirs))

;;;###autoload
(defun lib-autoload-create-and-update-file (dir target &optional forcep)
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
                             "\n")))
      (byte-compile-file target))))

(provide 'lib-autoload)
;;; lib-autoload.el ends here
