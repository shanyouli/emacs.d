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
(when (< emacs-major-version 27)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (unless (and (boundp IS-MAC) IS-MAC)
    (push '(menu-bar-lines . 0) default-frame-alist)))

(when t
  (run-with-idle-timer 3 nil
                       (function
                        (lambda
                            (&rest _)
                         :defer 3
                         (setq tool-bar-mode nil scroll-bar-mode nil)
                         (unless IS-MAC
                           (setq menu-bar-mode nil))))))

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

(add-hook! 'emacs-startup-hook
  :if (display-graphic-p)
  #'md/frame-default-size)
   ;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(add-hook! 'after-make-frame-functions
  :if (display-graphic-p)
  #'md/frame-size-after-make-frame-func+)

(add-hook! 'emacs-startup-hook
  (autoload 'winner-mode "winner")
  (winner-mode +1))

;; font
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook! 'after-make-frame-functions #'lye/font-initialize-frame+)
  (lye/font-initialize+))
(run-with-idle-timer! :defer 3 (lye/UI-module-install "cnfonts"))

;; mode-line
(add-hook! 'after-init-hook (md-modeline-mode +1))
;; (lye/UI-module-install "doom-modeline")
;; or
;; (lye/UI-module-install "awesome-tray")

;; header-line, Tab
;; (lye/UI-module-install "awesome-tab")

(provide 'core-ui)

;;; core-ui.el ends here
