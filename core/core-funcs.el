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

;;
(defun load-all-module-file ()
  "Import all el files in lye-emacs-modules-dir."
  (let* ((parent-dir lye-emacs-modules-dir)
         (default-directory parent-dir))
    (dolist (file (directory-files parent-dir))
      (unless (string-match "^\\." file )
        (load (expand-file-name file lye-emacs-modules-dir) )))))

(provide 'core-funcs)

;;; core-funcs.el ends here
