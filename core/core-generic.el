;;; core-generic.el --- Initialize the basic configuration -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli
;; Keywords: keywords


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

;; commentary

;;; Code:

;; Personal information
(setq user-full-name    lye-full-name
      user-mail-address lye-mail-address)

;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil #'(lambda () (bury-buffer)))

(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(blink-cursor-mode -1)                  ;指针不闪动
(transient-mark-mode 1)                 ;标记高亮
(global-subword-mode 1)                 ;Word移动支持 FooBar 的格式
(setq use-dialog-box nil)               ;never pop dialog
(setq inhibit-startup-screen t)         ;inhibit start screen
;;(setq initial-scratch-message "") ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq profiler-report-cpu-line-format ;让 profiler-report 第一列宽一点
      '((100 left)
        (24 right ((19 right)
                   (5 right)))))
(setq profiler-report-memory-line-format
      '((100 left)
        (19 right ((14 right profiler-format-number)
                   (5 right)))))

;; Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (require 'noflet)
  (noflet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; Edit
(setq-default major-mode 'text-mode)    ;设置默认地主模式为TEXT模式

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
;; Optional
;; (setq locale-coding-system 'utf-8)
(unless system/windows
  (setq selection-coding-system 'utf-8))

;; Miscs
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if name are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t) ; Deleting file go to OS'trash floder
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after poping mark pops it again
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Store all temporal files in a temporal directory instead of being
;; disseminated in the $HOME directory
(setq-default
  ;; Tramp history
 tramp-persistency-file-name (concat lye-emacs-cache-dir "tramp")
 ;; Bookmark-default-file
 bookmark-default-file (concat lye-emacs-cache-dir "bookmarks")
 ;; SemanticDB files
 semanticdb-default-save-directory (concat lye-emacs-cache-dir "semanticdb")
 ;; url files
 url-configuration-directory (concat lye-emacs-cache-dir "url")
 ;; eshell files
 eshell-directory-name (concat lye-emacs-cache-dir "eshell")
;; Game score
 gamegrid-user-score-file-directory (concat lye-emacs-cache-dir "games"))

;; @see https://emacs-china.org/t/spacemacs/9000
(setq auto-save-list-file-prefix nil ;not.# and #.# file
      auto-save-default nil ; not auto-save file
      make-backup-files nil ; not ~ file
      create-lockfiles nil) ; not .#*** file

(provide 'core-generic)

;;; core-generic.el ends here
