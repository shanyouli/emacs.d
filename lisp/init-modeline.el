;;; init-modeline.el --- modeline Initialize -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (doom-modeline)
;; Homepage: homepage
;; Keywords: modeline, UI


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

;; GUI and CUI

;;; Code:

(defvar default-modeline-format mode-line-format
  "Default mode-line format.")

;; Prevent flash of unstyled modeline at startup
(unless after-init-time
  (setq mode-line-format nil))

(if (locate-library "doom-modeline")
     (use-package doom-modeline
       :ensure nil
       :hook (after-init . doom-modeline-mode)
       :init
       (unless (display-graphic-p)
         (setq doom-modeline-icon nil))

       (setq doom-modeline-major-mode-color-icon t
             doom-modeline-minor-modes nil
             doom-modeline-mu4e nil
             doom-modeline-github t
             doom-modeline-github-interval 300)

       (setq doom-modeline-buffer-file-name-style 'truncate-upto-root))

   (add-hook 'after-init-hook
             (lambda ()
               (setq mode-line-format default-modeline-format))))
;; (load "~/.emacs.d/site-lisp/awesome-tray/awesome-tray.el")
;; (awesome-tray-mode 1)
;;; Display line, column and time on mode-line

;; Line and column
(setq column-number-mode t)
(setq line-number-mode t)

;; dispaly time
(unless (display-graphic-p)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (display-time-mode))

(provide 'init-modeline)

;;; init-modeline.el ends here
