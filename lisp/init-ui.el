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
                   %b))))
  (setq icon-title-format frame-title-format))

;; Suppress GUI features
(unless *is-a-term*
  (setq use-file-dialog nil
        use-dialog-box nil
        inhibit-startup-screen t))
(setq initial-major-mode 'emacs-lisp-mode
      initial-buffer-choice nil)

;; Window size and features
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame)
                                         1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; Make *Scratch* buffer undelete
(defun lye/unkillable-scratch-buffer ()
  "Don't delete *Scratch*."
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
	(delete-region (point-min) (point-max))
	(insert initial-scratch-message)
	nil)
    t))
(add-hook 'kill-buffer-query-functions
	  #'lye/unkillable-scratch-buffer)

;; Theme

;;;Setting a Hook run after a color theme is loaded using `load-theme'.
(defun after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))


;; set mode-line
;; (when (display-graphic-p)
;;   (quelpa '(awesome-tray :fetcher github :repo "manateelazycat/awesome-tray"))
;;   (use-package awesome-tray
;;     :ensure nil
;;     :commands awesome-tray-mode
;;     :hook (after-init . awesome-tray-mode)))

;; Theme
(defun standardize-theme (theme)
  "Standardize THEME."
  (pcase theme
    ('default 'monokai)
    ('light 'tao)
    ))
(defun lye-load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme:"
			     '(default light dark)))))
  (pcase theme
    ('default
     (use-package monokai-theme
       :init (load-theme 'monokai t)))
    ('light
     (use-package tao-theme
       :init (load-theme 'tao-yang t)))
    ('dark
     (use-package dakrone-theme
       :init (load-theme 'dakrone t)))))

;; Understand the topics currently in use
(defun current-theme ()
  "what is the Current theme?"
  (interactive)
  (message "The Current theme is %s"
           (substring (format "%s" custom-enabled-themes) 1 -1)))

(if (display-graphic-p)
    (lye-load-theme lye-themes)
  (require 'init-theme))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq track-eol t) ; keep cursor at end of lines, Require line-move-visual is nil
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don't compact font caches during GC.

;; set font
;; @see https://emacs-china.org/t/emacs/7268/2
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
;;(set-font "Source Code Pro" "simsun" 12 14)
(when (display-graphic-p)
  (set-font "Sarasa Mono T SC" "Sarasa Mono T SC" 13 13))

(provide 'init-ui)
;;; init-ui.el ends here
