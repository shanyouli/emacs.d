;;; iex-fuz.el --- fuzzy-match -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (fuz)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: fuzzy match


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Fuzzy Match

;;; Code:

;; TODO: Use strap.el instead of package.el

;; Because straight.el does not support dynamic modules, every time the emacs
;; version changes, it will require recompilation.

(require 'fuz)


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

(provide 'iex-fuz)

;;; iex-fuz.el ends here
