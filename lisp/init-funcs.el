;;; init-funcs.el --- Define functions.              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
;; Keywords:

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

;; Define functions.

;;; Code:



;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
(bind-keys ("C-M-r" . revert-current-buffer))

;; Save a file as utf-8
(defun save-buffer-as-utf-8 (coding-system)
  "Revert a buffer with 'CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system 'utf-8)
  (save-buffer))

;; Save a file as gbk
(defun save-buffer-with-gbk (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as gbk."
  (interactive "zcoding system for visited file (default nil):")
  (revert-buffer-with-coding-system 'gbk)
  (save-buffer))

;; Revert a buffer as gbk
(defun revert-buffer-with-gbk ()
  "Reopen the buffer in gbk format."
  (interactive)
  (revert-buffer-with-coding-system 'gbk))

;; Revert a buffer as utf-8
(defun revert-buffer-with-utf-8 ()
  "Reopen the buffer in utf-8 format."
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))

;; Recompile elpa directory
(defun recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-file-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-fle-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide 'init-funcs)
;;; init-funcs.el ends here
