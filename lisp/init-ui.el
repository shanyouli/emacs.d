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

;;; Variable

(defvar lye-frame-default-height nil
  "The ratio of the default height to the screen height is 0.618.")
(defvar lye-frame-default-width nil
  "The default width is half of the screen, and the error is between 2px.")

;;; funtcion

(defun lye/restore-frame-size (&optional frame)
  "Frame default size configuration."
  (interactive)
  (when frame (select-frame frame))
  (if ( and (boundp system/windows) system/windows)
      (progn
        (set-frame-width (selected-frame) lye-frame-default-width)
        (set-frame-height (selected-frame) lye-frame-default-height))
    (set-frame-size (selected-frame)
                    lye-frame-default-width lye-frame-default-height)))

(defun lye/init-default-frame-size ()
  "The frame default size setting at startup."
  ;; Set the initial window size
  ;; @see http://kimi.im/2019-02-09-emacs-frame-dimention
  ;; top, left ...
  (add-to-list 'default-frame-alist
               (cons 'top (/ (* 191 (x-display-pixel-height)) 1000)))
  (add-to-list 'default-frame-alist
               (cons 'left (/ (* 1 (x-display-pixel-height)) 2)))
  (add-to-list 'default-frame-alist
               (cons 'height lye-frame-default-height))
  (add-to-list 'default-frame-alist
               (cons 'width lye-frame-default-width)))

;;; Configuration

;; (setq facy-splash-image logo) ; Logo

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
  (if (and (boundp system/mac) system/mac)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (set-frame-parameter frame 'menu-bar-lines
                                       (if (display-graphic-p frame) 1 0))))
    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))))

;; starup frame size at GUI
(when (display-graphic-p)
  (setq lye-frame-default-height (/ (* 618 (x-display-pixel-height))
                                    (* 1000 (frame-char-height))))
  (setq lye-frame-default-width (- (/ (x-display-pixel-width)
                      (* 2 (frame-char-width))) 2))
  (lye/init-default-frame-size)  ; Frame Size after startup
  ;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
  (add-hook 'after-make-frame-functions #'lye/restore-frame-size))

;; Misc
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(setq inhibit-startup-screen t) ; 不展示开始界面

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq initial-buffer-choice nil)


(provide 'init-ui)
;;; init-ui.el ends here
