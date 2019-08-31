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
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; I generally prefer to hide the menu bar, but doing this on OS X
  ;; simply makes it update unreliably in GUI frames, so we make an
  ;; execption.
  (if (and (boundp system/mac) system/mac)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (set-frame-parameter frame 'menu-bar-lines
                                       (if (display-graphic-p frame) 1 0))))
    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))))

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq initial-buffer-choice nil)

;; Misc
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(setq inhibit-startup-screen t) ; 不展示开始界面

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

;; @see https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/extensions/lazycat/fullscreen.el
(defun fullscreen ()
  "Fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun fullscreen-toggle ()
  "Toggle fullscreen status."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; @see https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-startup.el#L95
(defun lye/init-fullscreen ()
  "Init fullscreen "
  (if (featurep 'cocoa)
      (progn
        ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
        ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
        ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
        ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
        ;;
        ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
        ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
        ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
        ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
        (setq ns-use-native-fullscreen nil)
        (setq ns-use-fullscreen-animation nil)

        ;; 默认先最大化。
        (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

        (run-at-time "2sec" nil
                     (lambda ()
                       (toggle-frame-fullscreen)
                       )))

    ;; 非Mac平台直接全屏
    (fullscreen)))

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
  (if lye-init-fullscreen-p
      (lye/init-fullscreen)
    (add-hook 'after-init-hook
              '(lambda ()
                 (run-with-idle-timer 0.1 nil #'lye/restore-frame-size))))

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
       (string-match "^\\ \\*" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*flycheck-posframe-buffer*" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))

  (setq awesome-tab-hide-tab-function 'lye/awesome-tab-hide-tab)

  (setq awesome-tab-face-height 130)

  (add-hook 'after-init-hook
            (lambda ()
              (lye/modules-require 'iex-all-the-icons)
              (awesome-tab-mode)

              (when (boundp 'after-load-theme-hook)
                (add-hook 'after-load-theme-hook
                          (lambda ()
                            (when (awesome-tab-mode-on-p)
                              (awesome-tab-mode -1)
                              (awesome-tab-mode 1))))))))

(provide 'core-ui)

;;; core-ui.el ends here
