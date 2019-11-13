;;; core/autoload/module.el -*- lexical-binding: t -*-

;;;###autoload
(defmacro lye/modules-require (pkg)
  "Import the *.el file in the lye-emacs-modules-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-emacs-modules-dir ,pkg)))

(defmacro load-nomessage (load-file)
  `(load ,load-file :no-error :no-message))

(defun lye/modules-install (dir pkg &optional configp)
  "Loading or installation configuration in a directory."
  (let* ((dir (if (string= dir (file-truename dir))
                 dir
               (expand-file-name dir lye-emacs-modules-dir)))
         (pkg-dir (expand-file-name pkg (file-name-as-directory dir))))
    (when (file-exists-p pkg-dir)
      (let ((pkg-install-file (format "%s/%s" pkg-dir "packages.el"))
            (pkg-config-file (format "%s/%s" pkg-dir "config.el")))
        (when (file-exists-p pkg-install-file)
          (load-nomessage pkg-install-file))
        (unless (or configp (not (file-exists-p pkg-config-file)))
          (load-nomessage pkg-config-file))))))

;;;###autoload
(defun lye/UI-module-install (pkg &optional configp)
  (lye/modules-install "UI" pkg configp))

;;;###autoload
(defun lye/apps-module-install (pkg &optional configp)
  (lye/modules-install "apps" pkg configp))
