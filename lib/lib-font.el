;;; lib-font.el --- My Font Configuration            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:font

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

;; Chinese and english monospace-font Initialize

;;; Code:

(defgroup lib-font nil
  "Chinese and English MonoSpace-Font group."
  :group 'lib-font)

(defcustom lib-font-english nil
  "English font name."
  :type 'string
  :group 'lib-font)

(defcustom lib-font-chinese nil
  "Chinese font name."
  :type 'string
  :group 'lib-font)

(defcustom lib-font-default-size 14
  "Default font size."
  :type '(choise  ;;Corresponding to the pixel size and
          (const :tag "五号" 14)
          (const :tag "小四" 15)
          (const :tag "四号" 17)
          (const :tag "三号" 20)
          (const :tag "小二" 24)
          (const :tag "二号" 29)
          (const :tag "一号" 35)
          (const :tag "小初" 42)

          ;;And a corresponding font size value lbs
          (const :tag "五号" 10.5)
          (const :tag "小四" 11.0)
          (const :tag "四号" 13.0)
          (const :tag "三号" 14.5)
          (const :tag "小二" 18.0)
          (const :tag "二号" 22.0)
          (const :tag "一号" 26.0)
          (const :tag "小初" 32.0)
          (const :tag "NULL" nil))
  :group 'lib-font)

(defcustom lib-font-size-pair nil
  "The font size pair for (English-size . Chinese-size)."
  :type 'list
  :group 'lib-font)

(defcustom lib-font-size-alist nil
  "Customize font size-list."
  :type 'list
  :group 'lib-font)

(defun lib-font-exist-p (fontname)
  "Return t, The FONT-NAME font is exist."
  (if (find-font (font-spec :family fontname)) t nil))

(defvar lib-font--size-alist nil)
(defun lib-font--initialize-size-pair-list (english-font &optional pixelp)
  "Initialize the list of `lib-font--size-alist'."
  (setq lib-font--size-alist
        (or lib-font-size-alist
            (pcase english-font
              ("Fantasque Sans Mono"
               (if pixelp
                   '((14 . 14) (15 . 16) (17 . 18) (20 . 20)
                     (24 . 24) (29 . 30) (35 . 36) (42 . 44))
                 '((10.5 . 10.5) (11.0 . 12.0) (13.0 . 13.5) (14.5 . 15.0)
                   (18.0 . 18.0) (22.0 . 22.5) (26.0 . 27.0) (32.0 .  33.0))))
              ("Fira Code"
               (if pixelp
                   '((14 . 18) (15 . 18) (17 . 20) (20 . 24)
                     (24 . 30) (29 . 36) (35 . 44) (42 . 52))
                 '((10.5 . 13.5) (11.0 . 13.5) (13.0 . 15.0) (14.5 . 18.0)
                   (18.0 . 22.5) (22.0 . 27.0) (26.0 . 33.0) (32.0 .  39.0))))
              (english-font
               (if pixelp
                   '((14 . 18) (15 . 18) (17 . 17) (20 . 20)
                     (24 . 24) (29 . 29) (35 . 35) (42 . 42))
                 '((10.5 . 10.5) (11.0 . 11.0) (13.0 . 13.0) (14.5 . 14.5)
                   (18.0 . 18.0) (22.0 . 22.0) (26.0 . 26.0) (32.0 .  32.0))))))))

(defun lib-font-set-size-pair (&optional english-font)
  "Retrun english and chinese size. `(english . chinese)'."
  (let ((eng-font (or english-font lib-font-english))
        (pixelp (integerp lib-font-default-size)))
    (lib-font--initialize-size-pair-list eng-font pixelp))
  (unless lib-font-size-pair
    (setq lib-font-size-pair (assoc lib-font-default-size lib-font--size-alist))))

(defun lib-font-set-monospace (english-font chinese-font english-size chinese-size)
  "Set the monospace font size when mixed Chinese and English words."
  (if (lib-font-exist-p english-font)
      (set-face-attribute 'default nil
                          :font (font-spec :family english-font :size english-size))
    (warn "This %s font is not found." english-font))
  (if (lib-font-exist-p chinese-font)
      (set-fontset-font t '(#x4e00 . #x9fff)
                        (font-spec :family chinese-font :size chinese-size))
    (warn "This %s font is not found." chinese-font)))

(defun lib-font-initialize-monospace ()
  "Initialize monspace-font."
  (let ((en lib-font-english)
        (zh lib-font-chinese)
        en-size zh-size)
    (lib-font-set-size-pair en)
    (setq en-size (car lib-font-size-pair)
          zh-size (cdr lib-font-size-pair))
    (lib-font-set-monospace en zh en-size zh-size)))

(defun lib-font--change-size (status)
  "When STATUS > 0, Increase font size. When STATUS < 0, Decrease font size."
  (let ((scale-steps lib-font--size-alist)
        (old-size-pair lib-font-size-pair)
        new-size-pair)
    (when (< step 0)
      (setq scale-steps (reverse scale-steps)))
    (setq new-size-pair (or (cadr (member old-size-pair scale-steps))
                            old-size-pair))
    (if (equal (car new-size-pair) (car old-size-pair))
        (if (< step 0)
            (message "Do not decrease font, and has reached a minimum font.")
          (message "Do not increase the font, it has a maximum."))
      (setq lib-font-size-pair new-size-pair)
      (lib-font-initialize-monospace)
      (lib-font/display-font-size))))

(defun lib-font/display-font-size ()
  "Display font-size."
  (interactive)
  (let ((en lib-font-english)
        (zh lib-font-chinese)
        (en-size (car lib-font-size-pair))
        (zh-size (cdr lib-font-size-pair)))
    (if (integerp en-size)
        (message "The font pixelsize of `%s' is %d and the pixelsize of `%s' is %d!"
                 en en-size zh zh-size)
      (message "The font size of `%s' is %.1f and the pixelsize of `%s' is %.1f!"
               en en-size zh zh-size))))

(defun lib-font/increase-font-size ()
  "Increase font-size acording `lib-font--size-alist'."
  (interactive)
  (lib-font--change-size +1))

(defun lib-font/decrease-font-size ()
  "Decrease font-size acroding `lib-font--size-alist'."
  (interactive)
  (lib-font--change-size -1))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

(provide 'lib-font)
;;; lib-font.el ends here
