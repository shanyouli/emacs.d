;;; bundles/term/vterm.el -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(defun vterm-module-compile-a (orig-fun &rest args)
  "This function compiles the vterm-module."
    (if (file-exists-p "/usr/lib64/libvterm.a")
      (let ((dir (file-name-directory (locate-library "vterm"))))
        (lye//run-command-with-buf
         "sh" "vterm-compile-buffer" dir "-c"
         "mkdir -p build;                            \
          cd build;                                  \
          cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo    \
               -DUSE_SYSTEM_LIBVTERM=/usr/lib64 ..;  \
          make"))
      (apply orig-fun args))
    (if-let ((vterm-module-file (locate-library "vterm-module")))
        (unless (string-prefix-p dynamic-module-dir vterm-module-file)
          (copy-file vterm-module-file dynamic-module-dir t)
          (delete-file vterm-module-file))))

(advice-add #'vterm-module-compile :around #'vterm-module-compile-a)
