;;; core/autoload/site-lisp.el -*- lexical-binding: t -*-

;;;###autoload
(defvar lye-site-lisp-autoload-file
  (expand-file-name "site-lisp-loadfs.el" lye-emacs-autoload-dir)
  "lye-emacs-site-lisp-dir directory automatically generated autoload file.")

;;;###autoload
(defun +add-site-lisp-to-load-path ()
  "Add Lye-emacs-site-lisp-dir to the `load-path'"
  (mapc #'add-to-load-path
        (+find-subdir-recursively lye-emacs-site-lisp-dir)))

;;;###autoload
(defun site-lisp-autoload-file-refresh ()
  (interactive)
  (generate-autoload-and-refresh lye-emacs-site-lisp-dir lye-site-lisp-autoload-file))

;;;###autoload
(defun +site-lisp-initialized ()
  "Initialization site-lisp-autoload-file."
  (unless (file-exists-p lye-site-lisp-autoload-file)
    (site-lisp-autoload-file-refresh)
    (load lye-site-lisp-autoload-file :no-error :no-message)))
