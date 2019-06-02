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

;; set font
;; @see https://emacs-china.org/t/emacs/7268/2
(defun lye/set-font (english chinese  &optional english-size chinese-size)
  (if english-size
      (set-face-attribute 'default nil :font
                          (font-spec :family english :size english-size))
    (set-face-attribute 'default nil :font english))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (if chinese-size
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size chinese-size))
      (set-fontset-font (frame-parameter nil 'font) charset chinese))))

  ;;Chinese and English font alignment
(defun lye/monospaced-chinese-and-english-fonts ()
  ;;  (interactive)
  ;; FantasqueSansMono and chinese font
  (if (member "Fantasque Sans Mono" (font-family-list))
      (if (member "Sarasa Term SC" (font-family-list))
          (lye/set-font "Fantasque Sans Mono" "Sarasa Mono SC" 11.5 12.0)

        (catch 'loop
          (dolist (font '("Sarasa Mono SC"
                          "WenQuanYi Micro Hei"
                          "Source Han Sans SC"
                          "Hiragino Sans GB"
                          "Noto Sans Mono CJK SC"))
            (when (member font (font-family-list))
              (lye/set-font "Fantasque Sans Mono" font 11.5 12.0)
              (throw 'loop t)))))

    ;; Fira Code, Hack, Source Code Pro
    (unless (catch 'loop1
              (dolist (en-font '("Fira Code" "Hack" "Source Code Pro"))
                (when (member en-font (font-family-list))
                  (catch 'loop
                    (dolist (ch-font '("Sarasa Mono SC"
                                       "WenQuanYi Micro Hei"
                                       "Hiragino Sans GB"
                                       "Source Han Sans SC"
                                       "Noto Sans Mono CJK SC"))
                      (when (member ch-font (font-family-list))
                        (lye/set-font en-font ch-font 10.5 12.0)
                        (throw 'loop t))))
                  (throw 'loop1 t))))
      (setq lye-enable-zh-and-en-same-width t))))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

(when (display-graphic-p)
  (lye/monospaced-chinese-and-english-fonts)

  ;; Specify fonts for symbol characters
  (cond
   ((member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend))
   ((member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)))

  ;; Spectify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t))))

  ;; Single font setting instead of Chinese and English width settings
  (when lye-enable-zh-and-en-same-width
    (catch 'loop
      (dolist (font '("Fira Code" "Hack" "DejaVu Sans Mono" "Source Code Pro"))
        (when (member font (font-family-list))
          (set-face-attribute 'default nil :font font
                              :height (cond
                                       (system/mac 130)
                                       (system/windows 110)
                                       (t 100)))
          (throw 'loop t))))

    ;; Specify font for Chinese Characters
    (catch 'loop
      (dolist (font '("WenQuanYi Micro Hei" "Microsoft Yahei"
                      "Noto Sans Mono CJK SC"))
        (when (member font (font-family-list))
          (set-fontset-font t 'han font nil 'append)
          (throw 'loop t)))))
  )

;;; Frame Size

;; Set the initial window size
(setq initial-frame-alist
      '((width . 86) (height . 32)))

(defun lye/reset-frame-size (&optional frame)
  "set the frame-size."
  (interactive)
  (when frame (select-frame frame))
  (if system/windows
      (progn
        (set-frame-width (selected-frame) 86)
        (set-frame-height (selected-frame) 32))
    (set-frame-size (selected-frame) 86 32)))
;; see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(add-hook 'after-make-frame-functions #'lye/reset-frame-size)

;; toggle-fullscreen

(defun lye/toggle-fullscreen ()
  (interactive)
  (if lye-toggle-fullscreen
      (let ((font "Sarasa Mono SC"))
        (setq lye-toggle-fullscreen nil)
        (toggle-frame-fullscreen)
        (when (member font (font-family-list))
          (lye/set-font font  font 14 14))
        )
    (setq lye-toggle-fullscreen t)
    (lye/monospaced-chinese-and-english-fonts)
    (toggle-frame-fullscreen)
    ))
(global-unset-key [f11])
(global-set-key (kbd "<f11>") 'lye/toggle-fullscreen)
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
