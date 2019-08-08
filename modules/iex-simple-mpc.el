;;; iex-simple-mpc.el --- Mpc -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (simple-mpc)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: music


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

;; Simple-Mpc

;;; Code:

(when (and (executable-find "mpc") (executable-find "mpd"))
  (require-package 'simple-mpc)
  (require 'simple-mpc)

  (defvar one-key-menu-simple-mpc-alist nil
          "The `one-key' menu alist for SIMPLE-MPC.")

  (setq one-key-menu-simple-mpc-alist
        '((("t" . "Toggle Music") . simple-mpc-toggle)
          (("n" . "Next Music")   . simple-mpc-next)
          (("p" . "Prev Music")   . simple-mpc-prev)
          (("f" . "Music Fast forward") . simple-mpc-seek-forward)
          (("b" . "Music backward") . simple-mpc-seek-backward)
          (("i" . "Inc vol") . simple-mpc-increase-volume)
          (("d" . "Dec Vol") . simple-mpc-decrease-volume)))

  (defun one-key-menu-simple-mpc ()
    "The `One-key' menu for simple-mpc."
    (interactive)
    (one-key-menu "SIMPLE-MPC" one-key-menu-simple-mpc-alist t t))


  ;; hydra key
  (defhydra hydra-simple-mpc-menu (:exit nil)
    ""
    )
  )

(provide 'iex-simple-mpc)

;;; iex-simple-mpc.el ends here
