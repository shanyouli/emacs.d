;;; core/autoload/module.el -*- lexical-binding: t -*-

;;;###autoload
(defvar lye-modules-autoload-file
  (expand-file-name "moudles-loadfs.el" lye-emacs-autoload-dir)
  "lye-modules-autoload-file directory automatically generated autoload file.")

;;;###autoload
(defun module-autoload-file-refresh ()
  (interactive)
  (generate-autoload-and-refresh lye-emacs-modules-dir lye-modules-autoload-file))

;;;###autoload
(defun +module-initialized ()
  "Initialization site-lisp-autoload-file."
  (unless (file-exists-p lye-modules-autoload-file)
    (module-autoload-file-refresh)
    (load lye-modules-autoload-file :no-error :no-message)))

;;;###autoload
(defmacro lye/modules-require (pkg)
  "Import the *.el file in the lye-emacs-modules-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-modules-dir ,pkg)))

;;;###autoload
(defun lye/module-install-UI (pkg)
  "Loading the installation package of the UI needs."
  (let ((UI-direcotry (expand-file-name pkg (format "%s%s" lye-emacs-modules-dir "UI"))))
    (when (file-directory-p UI-direcotry)
      (load (format "%s/%s" UI-direcotry "packages.el") :no-error :no-message)
      (load (format "%s/%s" UI-direcotry "config.el") :no-error :no-message))))
