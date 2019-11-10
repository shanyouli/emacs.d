;;; core-ui.el --- Initialize UI -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
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

  (unless (and (boundp system/mac) system/mac)
    (push '(menu-bar-lines . 0) default-frame-alist)))

(run-at-time 1 nil (lambda ()
                     (setq tool-bar-mode nil
                           scroll-bar-mode nil)
                     (unless  system/mac
                       (setq menu-bar-mode nil))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      initial-buffer-choice nil
      ;;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
      frame-resize-pixelwise t)

;; THEME
(setq mdt-theme-light-and-dark '(doom-one doom-molokai)
      ;;mdt-theme-switch-time '("08:30" . "18:00")
      mdt-theme-switch-time '(22.357 . 114.117)
      )
(add-hook 'after-init-hook (lambda ()
                             (require 'doom-themes)
                             (mdt/switch-light-or-dark-theme+)))

(when (display-graphic-p)
  (add-hook 'emacs-startup-hook #'md/frame-default-size)
   ;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
  (add-hook 'after-make-frame-functions #'md/frame-size-after-make-frame-func+))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'lye/font-initialize-frame+)
  (lye/font-initialize+))

;; mode-line
;; (lye/modules-require 'lex-modeline)
;; (if (display-graphic-p)
;;     (progn
;;       (lye/modules-require 'iex-awesome-tray)
;;       (+awesome-tray-initialize))
;;   (setq-default mode-line-format lex-modeline-format))

(provide 'core-ui)

;;; core-ui.el ends here
