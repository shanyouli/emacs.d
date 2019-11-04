;;; core/autoload/module.el -*- lexical-binding: t -*-

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
