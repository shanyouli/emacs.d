;;; lex-awesome-tab.el --- Initialize awesome-tab -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (awesome-tab)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tabber


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

;; awesome-tab

;;; Code:
(unless system/windows
  (lye/modules-require 'iex-all-the-icons))

;; awesome-tab style: slant, wave,alternate,bar,box,chamfer,rounded,zigzag
(setq awesome-tab-style 'zigzag)

(defun lye/awesome-tab-hide-tab (x)
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
     (string-prefix-p "*scratch*" name)
     (string-prefix-p "*One-Key*" name)
     (string-match "^\\ \\*" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*flycheck-posframe-buffer*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

(setq awesome-tab-hide-tab-function 'lye/awesome-tab-hide-tab)

;; (setq awesome-tab-face-height 130)

(defun lye/refresh-awesome-tab-mode ()
  "Refresh `awesome-tab-mode', especially after replacing themes."
  (when (awesome-tab-mode-on-p)
    (awesome-tab-mode -1)
    (awesome-tab-mode +1)))

(provide 'lex-awesome-tab)

;;; lex-awesome-tab.el ends here
