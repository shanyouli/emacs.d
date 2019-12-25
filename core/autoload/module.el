;;; core/autoload/module.el -*- lexical-binding: t -*-

;;;###autoload
(defmacro lye/modules-require (pkg)
  "Import the *.el file in the lye-modules-dir folder."
  `(require ,pkg (format "%s%s.el" ,lye-modules-dir ,pkg)))
