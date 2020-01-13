;;; core-generic.el --- Initialize the basic configuration -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.2
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

;;; remove global-subword-mode
;; time: 2019.09.02 16:48

;;; Code:

;; Personal information
(setq user-full-name    lye-full-name
      user-mail-address lye-mail-address)

;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      )
(fset #'display-startup-echo-area-message #'ignore)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;;
;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's boon.
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particcularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(fset 'yes-or-no-p 'y-or-n-p)           ; 以 y/n代表 yes/no
(blink-cursor-mode -1)                  ; 指针不闪动
(transient-mark-mode +1)                 ; 标记高亮
(setq use-dialog-box nil)               ; never pop dialog
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq default-major-mode 'text-mode)    ; 设置默认的主模式为TEXT
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏

;; Remove command line options that aren't relevant to our current OS; that
;;means less to process at startup.
(unless IS-MAC (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

(setq profiler-report-cpu-line-format   ;让 profiler-report 第一列宽一点
      '((100 left)
        (24 right ((19 right)
                   (5 right)))))
(setq profiler-report-memory-line-format
      '((100 left)
        (19 right ((14 right profiler-format-number)
                   (5 right)))))

;; Don't ask me when close emacs with process is running
(defun no-query-kill-emacs+ (orign &rest rest)
  "Prevent annoying \"Activ process exits\" query when you quit Emacs."
  (require 'noflet)
  (noflet ((process-list ()))
          (apply orign rest)))
(advice-add 'save-buffers-kill-emacs :around 'no-query-kill-emacs+)

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
;; Optional
;; (setq locale-coding-system 'utf-8)
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;;; Miscs

;; Show path if name are same
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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

;; 当光标到屏幕的倒数三行时,屏幕下移一行
(setq scroll-margin 3
      scroll-conservatively 1000000)

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
 gamegrid-user-score-file-directory (concat lye-emacs-cache-dir "games")
 ;; Saveplace
 save-place-file (concat lye-emacs-cache-dir "saveplace")
 ;; save-history
 savehist-file (concat lye-emacs-cache-dir "history")
 ;; Recentf-file
 recentf-save-file (concat lye-emacs-cache-dir "recentf")
 ;; server auth dir
 server-auth-dir (concat lye-emacs-cache-dir "server"))

;; @see https://emacs-china.org/t/spacemacs/9000
(setq auto-save-list-file-prefix nil ;not.# and #.# file
      auto-save-default nil ; not auto-save file
      make-backup-files nil ; not ~ file
      create-lockfiles nil) ; not .#*** file

;; Don't display `symbolic link to Git-controlled source file....'
;; @see https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks nil)

;; No display `*scratch*'
(run-with-idle-timer! :defer 0.2 (bury-buffer))
(add-hook! 'after-change-major-mode-hook
    (let ((buf "*scratch*"))
      ;; Avoid emacsclient opening *scratch* buffer and getting an error
      (unless initial-buffer-choice
        (setq initial-buffer-choice '(lambda () (switch-to-next-buffer))))
      (when (get-buffer buf) (kill-buffer buf))))

;;当在windows上运行时,确定 Msys2是否安装
(when IS-WINDOWS
  (require 'lib-winos))
(when (memq window-system '(x ns))
  (require 'lib-env)
  (setq lib-env-path-save-file (concat lye-emacs-cache-dir "env"))
  (lib-env-from-shell-initialize))

(advice-add 'find-file
            :before
            (lambda (arg1 &rest rest)
              (let ((d (file-name-directory arg1)))
                (unless (file-exists-p d)
                  (make-directory d t)))))

(provide 'core-generic)

;;; core-generic.el ends here
