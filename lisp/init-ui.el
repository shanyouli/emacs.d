;;; init-ui.el --- Initialize ui configurations.     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
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

;; UI

;;; Code:

;; Logo
;; (setq facy-splash-image logo)

;; Title
(when (display-graphic-p)
  (setq frame-title-format
        '("Lye Emacs - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   (buffer-name)))))
  (setq icon-title-format frame-title-format))

;; Window size and features
(when (version< emacs-version  "27.0.0")
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; I generally prefer to hide the menu bar, but doing this on OS X
  ;; simply makes it update unreliably in GUI frames, so we make an
  ;; exception.
  (if system/mac
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame) 1 0))))
    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))))

;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(defun lye/frame-heigh ()
  (/ (* 618 (x-display-pixel-height))
     (* 1000 (frame-char-height))))
(defun lye/frame-width ()
  (- (/ (x-display-pixel-width)
        (* 2 (frame-char-width)))
     2))


;;; Frame Size

;; Set the initial window size
;; @see http://kimi.im/2019-02-09-emacs-frame-dimention
(when (display-graphic-p)
  ;; top, left ...
  (add-to-list 'default-frame-alist
               (cons 'top (/ (* 191 (x-display-pixel-height)) 1000)))

  (add-to-list 'default-frame-alist
               (cons 'left (/ (* 1 (x-display-pixel-height)) 2)))
  (add-to-list 'default-frame-alist
               (cons 'height (/ (* 618 (x-display-pixel-height))
                                (* 1000 (frame-char-height)))))
  (add-to-list 'default-frame-alist
               (cons 'width (- (/ (x-display-pixel-width)
                                  (* 2 (frame-char-width)))
                               2))))
;; (setq initial-frame-alist
      ;; `((width . ,(lye/frame-width))
        ;; (height . ,(lye/frame-heigh))))

(defun lye/reset-frame-size (&optional frame)
  "set the frame-size."
  (interactive)
  (when frame (select-frame frame))
  (if system/windows
      (progn
        (set-frame-width (selected-frame) (lye/frame-width))
        (set-frame-height (selected-frame) (lye/frame-heigh)))
    (set-frame-size (selected-frame) (lye/frame-width) (lye/frame-heigh))))
;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(add-hook 'after-make-frame-functions #'lye/reset-frame-size)

;;; Misc
(setq ad-redefinition-action 'accept)  ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(fset 'yes-or-no-p 'y-or-n-p) ; 以 y/n 取代 yes/no
(setq inhibit-startup-screen t) ; 不展示开始界面
;;(setq visible-bell t)
(setq ring-bell-function 'ignore) ; Turn off the error ringtone
(setq mouse-yank-at-point t) ; Paste at the cursor position instead of the mouse pointer
(setq x-select-enable-clipboard t) ; 支持 emacs 和外部程序的粘贴
(setq track-eol t) ; keep cursor at end of lines, Require line-move-visual is nil
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don't compact font caches during GC.

;;; Line number
;; Line and column
(setq column-number-mode t)
(setq line-number-mode t)

;; dispaly time
(unless (display-graphic-p)
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (display-time-mode))

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq initial-buffer-choice nil)

(provide 'init-ui)
;;; init-ui.el ends here
