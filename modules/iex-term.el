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

;; @see https://github.com/akermu/emacs-libvterm#installation
(if (and (executable-find "make")
           (executable-find "libtool")
           (executable-find "cmake"))
    (progn
      (require 'vterm)
      (defun term-toggle ()
        (interactive)
        (if (member "vterm" (mapcar #'buffer-name (buffer-list)))
            (if (string= "vterm" (buffer-name (current-buffer)))
                (kill-buffer "vterm")
              (switch-to-buffer "vterm"))
          (vterm))))
  (lpm-install 'multi-term)
  (defalias 'term-toggle  'multi-term))

(provide 'iex-term)

;;; iex-term.el ends here
