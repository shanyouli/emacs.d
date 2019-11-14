;;; modules-winos.el --- Windows NT PATH -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: PATH


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

;; Environment variable configuration

;;; Code:

(defvar msys2-root  nil "The root directory of msys2.")

(defvar msys2-bin   nil "The executive of msys2.")

(defvar mingw64-bin nil "The executive of mingw64.")

(unless msys2-root
  (catch 'loop
    (dolist (mpath '("C:\\msys64"
                     "D:\\msys64"
                     "C:\\Applications\\msys64"
                     "D:\\Applications\\msys64"))
      (when (file-exists-p mpath)
        (setq msys2-root mpath)
        (throw 'loop t)))))

(when msys2-root
  (setq msys2-bin (concat msys2-root "\\usr\\bin"))
  (setq mingw64-bin (concat msys2-root "\\mingw64\\bin"))

  ;; Configure exec-path and PATH variables
  (setq exec-path (cons msys2-bin exec-path))
  (setq exec-path (cons mingw64-bin exec-path))

  (setenv "PATH" (concat msys2-bin ";" mingw64-bin ";" (getenv "PATH"))))

(provide 'modules-winos)

;;; modules-winos.el ends here
