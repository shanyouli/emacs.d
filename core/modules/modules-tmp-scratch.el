;;; moduless-tmp-scratch.el --- Substituted scratch-buffer -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tmp-scratch


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

;; `*scratch*' to replace the use of temporary files

;;; Code:

(defgroup modules-tmp-scratch nil
  "Tmp-Scratch module."
  :group 'modules-tmp-scratch)

(defcustom mts-directory
  (expand-file-name "tmp-scratch/"
                    (if (boundp 'lye-emacs-cache-dir)
                        lye-emacs-cache-dir
                      user-emacs-directory))
  "Temporary *scratch* Save Folder."
  :type 'directory
  :group 'modules-tmp-scratch)

;;;###autoload
(defun mts/scratch-initialize+ (&optional file)
  "Create a temporary buffer to replace *scratch*. Use `file' save it"
  (interactive)
  (unless (file-directory-p mts-directory)
    (make-directory mts-directory t))
  (let* ((file (expand-file-name (or file "mts.txt") mts-directory))
         (bf-list (mapcar 'buffer-name (buffer-list)))
         (file-buffer)
         (file-buffer-name))

     (cond ((string= "sh" (file-name-extension file))
            (if (member "*mts-sh*" bf-list)
                (setq file-buffer "*mts-sh*")
              (setq file-buffer (find-file file)
                    file-buffer-name "*mts-sh*")))
           ((string= "py" (file-name-extension file))
            (if (member "*mts-py*" bf-list)
                (setq file-buffer "*mts-py*")
              (setq file-buffer (find-file file)
                    file-buffer-name "*mts-py*")))
           ((string= "el" (file-name-extension file))
            (if (member "*mts-el*" bf-list)
                (setq file-buffer "*mts-el*")
              (setq file-buffer (find-file file)
                    file-buffer-name "*mts-el*")))
           (t
            (if (member "*mts-txt*" bf-list)
                (setq file-buffer "*mts-txt*")
              (setq file-buffer (find-file file)
                    file-buffer-name "*mts-txt*"))))
     (switch-to-buffer file-buffer)
     (when file-buffer-name
       (rename-buffer file-buffer-name))))

;;;###autoload
(defun mts/scratch-initialize-el ()
  "Temporary emacs-lisp test files."
  (interactive)
  (mts/scratch-initialize+ "mts.el"))

;;;###autoload
(defun mts/scratch-initialize-sh ()
  "Temporary sh-script test files."
  (interactive)
  (mts/scratch-initialize+ "mts.sh"))

;;;###autoload
(defun mts/scratch-initialize-py ()
  "Temporary Python-script test files."
  (interactive)
  (mts/scratch-initialize+ "mts.py"))

(provide 'moduless-tmp-scratch)

;;; moduless-tmp-scratch.el ends here
