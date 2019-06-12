;;; init-font.el ---Initialize Font Management            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:Font

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

;;

;;; Code:
;; https://gist.github.com/Superbil/7113937
(defvar emacs-english-font nil
  "The font name of English.")

(defvar emacs-cjk-font nil
  "The font name for CJK.")

(defvar emacs-font-size-pair nil
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list nil
  "This is used to store matching (english .chinese) font-size.")

(defun font-exist-p (fontname)
  "Test this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil
      t)))

(defun set-font (english chinese &optional size-pair)
  "Setup Emacs English and Chinese font on x window-system."
  (if size-pair
      (set-face-attribute 'default nil :font
                          (font-spec :family english :size (car size-pair)))
    (set-face-attribute 'default nil :font english))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (if size-pair
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair)))
      (set-fontset-font (frame-parameter nil 'font) charset chinese))))


(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0)
        (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (emacs-step-font-size -1))

;; Set the emacs font to `Sarasa Mono SC'
(defun lye/sarasa-font ()
  (if (or (font-exist-p "Sarasa Mono SC")
          (font-exist-p "Sarasa Term SC"))
      (progn
        (setq emacs-english-font "Sarasa Mono SC")
        (setq emacs-cjk-font "Sarasa Mono SC")
        (setq emacs-font-size-pair '(14 . 14))
        (setq emacs-font-size-pair-list
              '((10 . 10)  (12 . 12) (14 . 14)
                (16 . 16) (18 . 18) (19 . 19)
                (21 . 21) (22 . 22) (24 . 24)
                (26 . 26) (29 . 29) (32 . 32)
                (35 . 35) (38 . 38) (48 . 48)))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
        t)
    nil))

(defun lye/fantasque-sans-mono ()
  (if (font-exist-p "Fantasque Sans Mono")
      (progn
        (setq emacs-english-font "Fantasque Sans Mono")

        ;; set cjk font
        (catch 'loop
          (dolist (font '("Sarasa Mono SC"
                          "WenQuanYi Micro Hei"
                          "Hiragino Sans GB"
                          "Source Han Sans SC"
                          "Noto Sans Mono CJK SC"))
            (when (font-exist-p font)
              (setq emacs-cjk-font font)
              (throw 'loop t))))

        (setq emacs-font-size-pair '(11.5 . 12.0))
        (setq emacs-font-size-pair-list
              '((9.0 .  9.0)  (10.0 . 10.5) (11.5 . 12.0)
                (12.5 . 13.5) (14.0 . 15.0) (15.0 . 15.0)
                (16.0 . 16.5) (18.0 . 18.0) (20.0 . 21.0)
                (22.0 . 22.5) (24.0 . 25.5) (26.0 . 27.0)
                (28.0 . 28.5) (30.0 . 31.5) (32.0 . 33.5)))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
        t)
    nil))

;;; Hack, Fira Code, Source Code Pro, etc.
(defun lye/fc-hack-sourcep-font ()


  (let ((english-font-exist emacs-english-font))
    (catch 'loop
      (dolist (font '("Fira Code"
                      "Hack"
                      "Source Code Pro"))
        (when (font-exist-p font)
          (setq emacs-english-font font)
          (throw 'loop t))))

    (if (string= english-font-exist emacs-english-font)
      (progn
        ;; set cjk font
        (catch 'loop
          (dolist (font '("Sarasa Mono SC"
                          "WenQuanYi Micro Hei"
                          "Hiragino Sans GB"
                          "Source Han Sans SC"
                          "Noto Sans Mono CJK SC"))
            (when (font-exist-p font)
              (setq emacs-cjk-font font)
              (throw 'loop t))))

        (setq emacs-font-size-pair '(10.5 . 12.0))
        (setq emacs-font-size-pair-list
              '((9.0 .  10.5)  (10.5 . 12.0) (11.5 . 13.5)
                (12.0 . 15.0) (14.0 . 16.5) (15.0 . 18.0)
                (16.0 . 19.5) (18.0 . 21.0) (20.0 . 24.0)
                (22.0 . 25.5) (24.0 . 28.5) (26.0 . 31.5)
                (28.0 . 33.0) (30.0 . 36.0) (32.0 . 39.0)))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
        t)
    nil)))

(defun lye/load-font ()
  (cond
   ((lye/fantasque-sans-mono) t)
   ((lye/sarasa-font) t)
   ((lye/fc-hack-sourcep-font) t)
   (t nil)))

(when window-system
  ;; setup change size font, base on emacs-font-size pair-list
  (global-set-key (kbd "C-M-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C-M--") 'decrease-emacs-font-size)

  ;; setup default english font and cjk font.
  (unless (lye/load-font)
    (catch 'loop
      (dolist (font '("Fira Code" "Hack"
                      "DejaVu Sans Mono" "Source Code Pro"))
        (when (font-exist-p font)
          ;; (member font (font-family-list))
          (set-face-attribute 'default nil :font font
                              :height (cond
                                       (system/mac 130)
                                       (system/windows 110)
                                       (t 100)))
          (throw 'loop t))))

    (catch 'loop
      (dolist (font '("WenQuanYi Micro Hei"
                      "Microsoft Yahei"
                      "Noto Sans Mono CJK SC"))
        (when (font-exist-p font)
          (set-fontset-font t 'han font nil 'append)
          (throw 'loop t)))))

  ;; Specify fonts for symbol characters
  (cond
   ((font-exist-p "Apple Color Emoji")
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend))
   ((font-exist-p "Segoe UI Emoji")
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)))

  ;; Spectify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (font-exist-p font)
    (set-fontset-font t 'unicode font nil 'prepend)
    (throw 'loop t)))))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

(provide 'init-font)
;;; init-font.el ends here
