;;; core-awesome-tab.el --- Init Awesome-Tab -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (awesome-tab all-the-icons)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tab


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

;; Init awesome-tab

;;; Code:

(when (display-graphic-p)

  (setq awesome-tab-style 'slant) ; awesome-tab style

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
       (string-prefix-p " *which-key*" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*flycheck-posframe-buffer*" name)

     ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))

  (setq awesome-tab-hide-tab-function 'lye/awesome-tab-hide-tab)

  (add-hook 'after-init-hook
            '(lambda ()
                 (require 'awesome-tab)
                 (when (locate-library "all-the-icons") ; require all-the-icons
                   (require 'all-the-icons))
                 (awesome-tab-mode))))

(provide 'core-awesome-tab)

;;; core-awesome-tab.el ends here
