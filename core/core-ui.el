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

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq initial-buffer-choice nil)

;; Misc
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙

;; Frame Size
(defun lye/frame-default-height ()
  "The ratio of the default height to the screen height is 0.618."
  (/ (* 618 (x-display-pixel-height)) (* 1000 (frame-char-height))))

(defun lye/frame-default-width ()
  "The default width is half of the screen, and the error is between 2px."
  (- (/ (x-display-pixel-width) (* 2 (frame-char-width))) 2))

(defun lye/restore-frame-size (&optional frame)
  "Frame default size configuration."
  (interactive)
  (when (display-graphic-p)
    (when frame (select-frame frame))
    (if ( and (boundp system/windows) system/windows)
        (progn
          (set-frame-width (selected-frame) (lye/frame-default-width))
          (set-frame-height (selected-frame) (lye/frame-default-height)))
      (set-frame-size (selected-frame)
                      (lye/frame-default-width) (lye/frame-default-height)))))

;; THEME
(setq theme-switch-light      'doom-one-light
      theme-switch-dark       'doom-one
      theme-switch-light-time "08:30"
      theme-switch-dark-time  "19:30")
(add-hook 'after-init-hook (lambda ()
                             (require 'doom-themes)
                             (theme-switch-light-or-dark-theme)))

(when (display-graphic-p)
  ;; @see http://kimi.im/2019-02-09-emacs-frame-dimention
  ;; top, left ...
  ;; Set the initial window size
  (add-to-list 'default-frame-alist
               (cons 'top (/ (* 191 (x-display-pixel-height)) 1000)))
  (add-to-list 'default-frame-alist
               (cons 'left (/ (* 1 (x-display-pixel-height)) 2)))

  ;; Frame Size after startup
  (add-hook 'after-init-hook #'lye/restore-frame-size)

  ;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
  (add-hook 'after-make-frame-functions #'lye/restore-frame-size)

  ;; font
  (setq setup-english-font "Fantasque Sans Mono")
  (setq setup-cjk-font (cond
                        (system/linux
                         "WenQuanYi Micro Hei")
                        (system/mac
                         "Hiragio Sans GB")
                        (system/windows
                         "Microsoft Yahei")))
  (setq setup-font-default-size 14)
  (setup-font-initialize)

  ;; awesome-tab
  (add-hook 'after-init-hook
            (lambda ()
              (lye/modules-require 'lex-awesome-tab)
              (awesome-tab-mode +1)
              (add-hook 'after-load-theme-hook #'lye/refresh-awesome-tab-mode))))

(provide 'core-ui)

;;; core-ui.el ends here
