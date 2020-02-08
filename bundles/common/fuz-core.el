;;; bundles/common/fuz-core.el.el -*- lexical-binding: t -*-


(unless (require 'fuz-core nil t)
  (cond
   ((file-directory-p "/usr/lib/llvm/9")
    (setenv "LIBCLANG_PATH" "/usr/lib/llvm/9/lib64"))
   ((file-directory-p "/usr/lib/llvm/8")
    (setenv "LIBCLANG_PATH" "/usr/lib/llvm/8/lib64")))
  (fuz-build-and-load-dymod)
  (let ((old-file (locate-library "fuz-core")))
    (lye//move-file (lib-f-join
                     (file-name-directory old-file)
                     "target/release"
                     (cond (IS-LINUX "libfuz_core.so")
                           (IS-MAC "libfuz_core.dylib")
                           ((memq system-type '(windows-nt ms-dos cygwin))
                            "fuz_core.dll")))
                    (concat straight-dynamic-modules-dir
                            (file-name-nondirectory old-file)))
        (delete-file old-file)))
