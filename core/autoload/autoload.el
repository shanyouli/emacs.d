;;; core/autoload/autoload.el  -*- lexical-binding: t -*-

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
                    (file-exists-p (expand-file-name ".nosearch" d))))))

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
  "Building or refreshing `autoload'."
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
