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

(defun lib-autoload-create-and-update-file (dir target &optional forcep)
  "Autoload directory DIR generated for a file named TARGET in.
If the FORCEP is non-nil, forcibly regenerate a new DIR file autoload it."
  (require 'autoload)
  (let ((generated-autoload-file (file-name-sans-extension
                                  (file-name-nondirectory target)))
        (parent-dir (file-name-directory target)))
    (unless (file-exists-p parent-dir)
      (make-directory parent-dir t))
    (when (or forcep (not (file-exists-p target)))
      (with-temp-file target
        (dolist (f (lib-f-list-subfile dir))
          (let ((generated-autoload-load-name (file-name-sans-extension f)))
            (autoload-generate-file-autoloads f (current-buffer))))
        (insert (string-join `(,(char-to-string ?\C-l)
                               ";; Local Varibles:"
                               ";; version-control: never"
                               ";; no-byte-compile: t"
                               ";; no-update-autoloads: t"
                               ";; coding: utf-8"
                               ";; End:"
                               ,(format ";;; %s ends here"
                                        (file-name-nondirectory target)))
                             "\n"))))))

(provide 'lib-autoload)
;;; lib-autoload.el ends here
