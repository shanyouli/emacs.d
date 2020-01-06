;;; iex-term.el --- initialize Vterm -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (vterm)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: shell


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

;; Use vterm instead of term

;;; Code:

(defun lye//vterm-module-compile ()
  "This function compiles the vterm-module."
  (if (file-exists-p "/usr/lib64/libvterm.a")
      (let ((dir (file-name-directory (locate-library "vterm"))))
        (lye//run-command "sh" dir "-c"
                          "mkdir -p build;                            \
                           cd build;                                  \
                           cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo    \
                                 -DUSE_SYSTEM_LIBVTERM=/usr/lib64 ..; \
                           make")
        t)
    (vterm-module-compile)))

(defun lye//vterm-run ()
  (require 'vterm)
  (let ((vterm-module-file (locate-library "vterm-module"))
        result)
    (if vterm-module-file
        (unless (string= (file-name-directory vterm-module-file)
                         straight-dynamic-modules-dir)
          (setq result t))
      (setq result (lye//vterm-module-compile)))
    (when result
      (lye//move-file (locate-library "vterm-module")
                      straight-dynamic-modules-dir)))
  (vterm))

(setq shell-pop-shell-type
      (cond (lye-package--use-vterm
             '("vterm" "*vterm*" ;;(lambda () (require 'vterm) (vterm))
               #'lye//vterm-run))
            (t
             '("multi-term" "*Multi-TERM*" (lambda () (require 'multi-term) (multi-term))))))

(setq shell-pop-window-size 38)

(require 'shell-pop)

(provide 'iex-term)

;;; iex-term.el ends here
