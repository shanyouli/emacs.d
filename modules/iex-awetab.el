;;; iex-awetab.el --- Initialize awesome tab -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (awesome-tab all-the-icons)
;; Homepage: homepage
;; Keywords: keywords
;; Last-Updated: 2019-12-05 15:41:42


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

;;; Change log:
;;
;; 12/05/19

;;; Code:

(require 'awesome-tab)
;; awesome-tab style: slant, wave,alternate,bar,box,chamfer,rounded,zigzag
(setq awesome-tab-style 'zigzag)
(setq awesome-tab-hide-tab-function 'lye/awesome-tab-hide-tab+)
(if (boundp 'after-load-theme-hook)
    (add-hook 'after-load-theme-hook #'lye/refresh-awesome-tab-mode+))

(defun lye/awesome-tab-hide-tab+ (x)
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated windows.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below dedicated window.
     (string-prefix-p "*esup" name)
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (unless (get-buffer "*scratch*")
       (string-prefix-p "*scratch*" name))
     (string-prefix-p "*One-Key*" name)
     (string-prefix-p "*sdcv" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*flycheck-posframe-buffer*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

(defun lye/refresh-awesome-tab-mode+ ()
  "Refresh `awesome-tab-mode', especially after replacing themes."
  (when (awesome-tab-mode-on-p)
    (awesome-tab-mode -1)
    (awesome-tab-mode +1)))

(awesome-tab-mode +1)

(provide 'iex-awetab)

;;; iex-awetab.el ends here
