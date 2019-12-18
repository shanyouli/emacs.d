;;; core-ui.el --- Initialize UI -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: UI


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

;; UI

;;; Code:

;; (setq facy-splash-image logo) ; Logo

;; Title
(when (display-graphic-p)
  (setq frame-title-format
        '("Lye Emacs - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   (buffer-name)))))
  (setq icon-title-format frame-title-format))

;; Not scroll-bar, tool-bar and menu-bar-mode
(run-with-idle-timer! :defer 3
  (setq tool-bar-mode nil scroll-bar-mode nil)
  (unless IS-MAC
    (setq menu-bar-mode nil)))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      initial-buffer-choice nil
      ;;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
      frame-resize-pixelwise t)

;;
;;; THEME
(add-hook! 'after-init-hook
    (eval-when-compile (require 'doom-themes))
  (setq mdt-theme-light-and-dark '(doom-one doom-molokai)
        mdt-theme-switch-time '(30.93 . 113.92))
  (mdt/switch-light-or-dark-theme+))

;;
;;; Frame size
(when (display-graphic-p) (lib-frame-default-size))
;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(add-hook! 'after-make-frame-functions
 :if (display-graphic-p)
 #'lye|frame-default-size-with-frame)

(add-hook! 'emacs-startup-hook
  (autoload 'winner-mode "winner")
  (winner-mode +1))

;;
;;; font
(defcustom lye-en-font nil "Customize English font." :type 'string)
(defcustom lye-zh-font nil "Customize Chinese font." :type 'string)
(defcustom lye-default-font-size nil "Customize font size." :type 'integer)

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook! 'after-make-frame-functions #'lye|font-initialize)
  (lye-font-initialize))

;; mode-line
(add-hook! 'after-init-hook (md-modeline-mode +1))

(provide 'core-ui)

;;; core-ui.el ends here
