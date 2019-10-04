;;; core-funcs.el --- Some useful functions -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:


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

;; commentary

;;; Code:

;;;###autoload
(defun gzip-a-file (old-file new-file)
  (if (file-exists-p new-file)
      (message "%s already exists. If you want to extract to %s again, please rename %s"
               new-file new-file new-file)
    (if (not (executable-find "gzip"))
        (message "gzip don't exists, Please run the function after installing gzip.")
      (if (and (file-exists-p old-file)
               (member (file-name-extension old-file) (list "gz" "gzip")))
          (let* ((oldgzip (file-truename old-file))
                (newfile (file-truename new-file)))
            (unless (file-name-directory (file-truename new-file))
              (make-directory (file-name-directory new-file)))
            (shell-command-to-string
             (format "gzip -c -d %s > %s" old-file (file-truename new-file)))
            (message "Decompression is complete."))
        (message "%s is not a file compressed with gzip." old-file)))))

;;;###autoload
(defun download-a-file (URL file-name &optional cmd)
  (if  (string-match "\\/" file-name)
     (if (not (file-exists-p file-name))
         (let ((file (file-truename file-name)))
           (unless (file-exists-p (file-name-directory file))
             (make-directory file))
           (if cmd
               (cond ((and (executable-find cmd) (string= "axel" cmd))
                      (async-shell-command
                       (format "axel -n 20 %s -o %s" URL file))
                      ))
             (url-copy-file URL file))
           (message "%s download successful." file-name))
       (message "%s already exists, please re-determine the download file name."
                file-name))
    (message "Please use an absolute path.")))

;;;###autoload
(defun load-all-module-file ()
  "Import all el files in lye-emacs-modules-dir."
  (dolist (f (directory-files lye-emacs-modules-dir t "\\.el\\'"))
    (load f :no-error :no-message)))

;;;###autoload
(defun delete-same-element-in-list (list)
  "Delete the same element in a list."
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

;;;###autoload
(defun +find-subdir-recursively (dir)
  "Find all subdirectories in DIR.
Dot-directories and directories contain `.nosearch' will be skipped."
  (eval-when-compile (require 'subr-x)
                     (require 'cl-seq))

  (thread-last (directory-files dir nil)
    (cl-remove-if (lambda (f)
                    (string-prefix-p "." f)))
    (mapcar (lambda (d) (expand-file-name d dir)))
    (cl-remove-if-not #'file-directory-p)
    (cl-remove-if (lambda (d)
                    (file-exists-p (expand-file-name ".nosearch"
                                                     d))))))

;;;###autoload
(defun +find-el-file-recursively (dir)
  "Find all `.el' files in DIR and its subdirectories."
  (let ((elfiles (directory-files dir t "\\.el\\'"))
        (subdir (+find-subdir-recursively dir)))
    (nconc elfiles
           (mapcan #'+find-el-file-recursively subdir))))

;;;###autoload
(defun +get-dir-name-nondirectory (dir)
  "Get a DIR does not include the name of the upper path,
like: `~/.emacs.d/modules' ==> modules"
  (file-name-nondirectory (directory-file-name dir)))

;;;###autoload
(defun generate-autoload-and-refresh (dir &optional target)
  ""
  (require 'autoload)
  (let* ((dir dir)
         (target
          (or target (expand-file-name
                      (concat (+get-dir-name-nondirectory dir) "-loadfs.el")
                      lye-emacs-autoload-dir)))
         (generated-autoload-file target))
        (with-temp-file target
      (dolist (f (+find-el-file-recursively dir))
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
                           "\n")))))



(provide 'core-funcs)

;;; core-funcs.el ends here
