;;; modules-font.el --- Own font configuration -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: font


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

;; Font Management Configuration

;;; Code:

(defgroup modules-font nil
  "Font configuration module."
  :group 'modules-font)

(defcustom mdf-english nil
  "`English' font name. "
  :type 'string
  :group 'modules-font)

(defcustom mdf-cjk nil
  "Chinese font name."
  :type 'string
  :group 'modules-font)

(defcustom mdf-default-size 14
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
  :group 'modules-font)

(defcustom  mdf-size-pair nil
  "The font size pair for (english-size . chinese-size)"
  :type 'list
  :group 'modules-font)

(defvar mdf--size-pair-list nil  "Grow and shrink the font list.")

;;;###autoload
(defun mdf/font-exist-p! (fontname)
  "Test the font is exist or not"
  (if (and fontname (find-font (font-spec :family fontname))) t nil))

(defun mdf/size-pair-list-initialize! (english-font &optional use-pixel-p)
  "Initialize the list of `mdf--size-pair-list'"
  (cond
   ((string= english-font "Fantasque Sans Mono")
    (setq mdf--size-pair-list
          (if use-pixel-p
              '((14 . 14) (15 . 16) (17 . 18) (20 . 20)
                (24 . 24) (29 . 30) (35 . 36) (42 . 44))
            '((10.5 . 10.5) (11.0 . 12.0) (13.0 . 13.5) (14.5 . 15.0)
              (18.0 . 18.0) (22.0 . 22.5) (26.0 . 27.0) (32.0 .  33.0)))))
   ((string= english-font "Fira Code")
    (setq mdf--size-pair-list
          (if use-pixel-p
              '((14 . 18) (15 . 18) (17 . 20) (20 . 24)
                (24 . 30) (29 . 36) (35 . 44) (42 . 52))
            '((10.5 . 13.5) (11.0 . 13.5) (13.0 . 15.0) (14.5 . 18.0)
              (18.0 . 22.5) (22.0 . 27.0) (26.0 . 33.0) (32.0 .  39.0)))))
   (t
    (setq mdf--size-pair-list
          (if use-pixel-p
              '((14 . 18) (15 . 18) (17 . 17) (20 . 20)
                (24 . 24) (29 . 29) (35 . 35) (42 . 42))
            '((10.5 . 10.5) (11.0 . 11.0) (13.0 . 13.0) (14.5 . 14.5)
              (18.0 . 18.0) (22.0 . 22.0) (26.0 . 26.0) (32.0 .  32.0)))))
   ))

(defun mdf/set-size-pair+ (&optional english-font)
  "Get font size."
  (unless mdf-size-pair
    (let ((en-font (or english-font mdf-english)))
      (if (integerp mdf-default-size)
          (mdf/size-pair-list-initialize! en-font t)
        (mdf/size-pair-list-initialize! en-font)))
    (setq mdf-size-pair
          (assoc mdf-default-size mdf--size-pair-list))))

;;;###autoload
(defun mdf/monospace-font! (&optional english-font cjk-font size-pair)
  "Set the monospace font size when mixed CJK and English words."
  (let ((en-font (or english-font mdf-english))
        (cjk-font (or cjk-font mdf-cjk)))
    (unless size-pair
      (mdf/set-size-pair+ en-font)
      (setq size-pair mdf-size-pair))

    (if (mdf/font-exist-p! en-font)
        (set-face-attribute 'default nil :font
                            (font-spec :family en-font :size (car size-pair)))
      (warn "This %s font was not fond." en-font))

    (if (mdf/font-exist-p! cjk-font)
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family cjk-font :size (cdr size-pair))))
      (warn "This %s font was not found." cjk-font))))

(defun mdf/change-font-size! (step)
  "Increase/Descreasse/Display font size."
  (let ((scale-steps mdf--size-pair-list)
        (fsp mdf-size-pair))
    (if (< step 0)
        (setq scale-steps (reverse scale-steps)))

    (setq fsp (or (cadr (member fsp scale-steps)) fsp))
    (if (equal (car fsp) (car mdf-size-pair))
        (if (< step 0)
            (message "Do not decrease font, and has reached a minimum font.")
          (message "Do not enlarge the font, it has a maximum."))
      (setq mdf-size-pair fsp)
      (mdf/monospace-font!)
      (mdf/display-font-size))))

;;;###autoload
(defun mdf/increase-font-size ()
  "Increase font-size acording `mdf--size-pair-list'."
  (interactive)
  (mdf/change-font-size! 1))

;;;###autoload
(defun mdf/decrease-font-size ()
  "Decrease font-size acording `mdf--size-pair-list'."
  (interactive)
  (mdf/change-font-size! -1))

;;;###autoload
(defun mdf/display-font-size ()
  "Display font-size."
  (interactive)
  (if (integerp (car mdf-size-pair))
      (message "The font size of `%s' is %d and the size of `%s' is %d!"
               mdf-english (car mdf-size-pair) mdf-cjk (cdr mdf-size-pair))
    (message "The font size of `%s' is %.1f and the size of `%s' is %.1f!"
             mdf-english (car mdf-size-pair) mdf-cjk (cdr mdf-size-pair))))

;;;###autoload
(defun mdf/goto-default-size-font ()
  "Returns the default font size."
  (interactive)
  (unless mdf--size-pair-list
    (mdf/size-pair-list-initialize! mdf-english))
  (let ((size-pair (assoc mdf-default-size mdf--size-pair-list)))
    (mdf/monospace-font! nil nil size-pair)))

;;;###autoload
(defun mdf/monospace-font-initialize+ (&optional english-font cjk-font default-size)
  "Initialization English monospaced font."

  (if english-font
      (setq mdf-english english-font))
  (if cjk-font
      (setq mdf-cjk cjk-font))
  (when default-size
    (if (listp default-size)
        (setq mdf-size-pair default-size)
      (setq mdf-default-size default-size)))
  (if (and mdf-english mdf-cjk (or mdf-size-pair mdf-default-size))
      (mdf/monospace-font! mdf-english mdf-cjk mdf-size-pair)
    (message "Unable to set monospaced font!!")))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

(provide 'modules-font)

;;; modules-font.el ends here
