;;; lib-winos.el --- Configurations of windows-os    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Configurations of Window-System

;;; Code:

;; Emacs on Windows frequently confuses HOME (C:\Users\<NAME>) and APPDATA,
;; causing `abbreviate-home-dir' to produce incorrect paths.
(setq abbreviated-home-dir "\\'`")

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
;;Reduce the workload when doing file IO
(setq w32-get-true-fileattributes nil)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined.
;;使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches t)

;; Environment Variable Configuration
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

(provide 'lib-winos)
;;; lib-winos.el ends here
