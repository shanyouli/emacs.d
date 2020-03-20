;;; bundles/term/vterm.el -*- lexical-binding: t -*-

(defun lye//vterm-module-compile ()
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
    (vterm-module-compile)))

(with-eval-after-load 'vterm
  (let ((vterm-module-file (locate-library "vterm-module"))
        result)
    (if vterm-module-file
        (unless (string= (file-name-directory vterm-module-file)
                         straight-dynamic-modules-dir)
          (setq result t))
      (setq result (lye//vterm-module-compile)))
    (when result
      (lye//move-file (locate-library "vterm-module")
                      straight-dynamic-modules-dir))))
