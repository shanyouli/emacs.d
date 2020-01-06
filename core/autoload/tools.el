;;; core/autoload/tools.el -*- lexical-binding: t -*-

;;;###autoload
(defun download-the-file (URL file-name &optional cmd)
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
(defun ungzip-the-file (old-file new-file)
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
(defun lye//run-command (command &optional dir &rest args)
  "Call Process with COMMAND and ARGS in DIR."
  (let ((default-directory (or dir default-directory)))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process command nil t nil args))
        (error (buffer-string))))))

;;;###autoload
(defun lye//move-file (old new)
  (lib-f-make-parent-dir new)
  (lye//run-command "mv" nil "-vf" old new))
