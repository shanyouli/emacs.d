;;; core/autoload/core-modules.el -*- lexical-binding: t -*-


;;;###autoload
(defvar lye-core-modules-autoload-file
  (expand-file-name "core-modules-loadfs.el" lye-emacs-autoload-dir)
  "lye-emacs-site-lisp-dir directory automatically generated autoload file.")

;;;###autoload
;; (defun +add-site-lisp-to-load-path ()
;;   "Add Lye-emacs-site-lisp-dir to the `load-path'"
;;   (mapc #'add-to-load-path
;;         (+find-subdir-recursively lye-emacs-site-lisp-dir)))

;;;###autoload
(defun core-modules-autoload-file-refresh ()
  (interactive)
  (generate-autoload-and-refresh lye-emacs-core-modules-dir lye-core-modules-autoload-file))


;;;###autoload
(defun +site-lisp-initialized ()
  "Initialization site-lisp-autoload-file."
  (unless (file-exists-p lye-core-modules-autoload-file)
    (site-lisp-autoload-file-refresh)
    (load lye-core-modules--autoload-file :no-error :no-message)))

