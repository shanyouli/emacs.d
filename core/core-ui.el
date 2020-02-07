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
(defcustom lye-autoload-switch-dark-or-light-p nil
  "If it is non-nil, Not use `lib-theme-switch-theme'."
  :type 'boolean)
(defcustom lye-autoload-switch-theme-and-time nil
  "Default dark or light theme"
  :type 'list)
(defcustom lye-theme-list nil "Using theme list." :type 'list)
(defcustom lye-default-theme nil "Lye Default theme." :type 'list)

(defvar lye--current-theme nil)
(setq lye-autoload-switch-theme-and-time '((doom-one  doom-molokai) .
                                           (30.93 . 113.92)))
(add-hook! 'after-init-hook
    (eval-when-compile (require 'doom-themes))
  (lye|initialize-theme))

;;
;;; Frame size
(when (display-graphic-p) (lib-frame/default-size))
;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(add-hook! 'after-make-frame-functions
  :if (display-graphic-p)
  'lye|frame-default-size-with-frame)

;;
;;; font
(defcustom lye-en-font nil "Customize English font." :type 'string)
(defcustom lye-zh-font nil "Customize Chinese font." :type 'string)
(defcustom lye-default-font-size nil "Customize font size." :type 'integer)

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook! 'after-make-frame-functions #'lye|font-initialize)
  (lye-font-initialize))

;;
;;; mode-line
(defvar lye--default-modeline-format mode-line-format)
(require 'lib-modeline)

;;
;;; Line-Number
;; 文件超过10000行，不显示行号，只留4位吧
(setq display-line-numbers-width-start 4)
(defun lye-display-line-numbers ()
  "当文件的列宽 <86 或者 文件的 Major－mode 为 org－mode 且行数大于 1000,
不显示行号。"
  (let* ((edges (frame-edges))
         (width-start (nth 0 edges))
         (width-end (nth 2 edges))
         result)
    (when (< 86 (truncate (/ (- width-end width-start) (frame-char-width))))
      (setq result (or (not (eq major-mode 'org-mode))
                       (< (line-number-at-pos (point-max) 1000)))))
    (if result
        (display-line-numbers-mode +1)
      (display-line-numbers-mode -1))))

(dolist (h '(prog-mode-hook org-mode-hook))
  (add-hook! h (lye-display-line-numbers)))

(provide 'core-ui)

;;; core-ui.el ends here
