;;; core/autoload/font.el -*- lexical-binding: t -*-

;;;###autoload
(defvar lye-font-english-cjk-font nil
  "Confirm Font Name for `(english-font . chinese font)' or `english-font'")

;;;###autoload
(defvar lye-font-default-size 14
  "Default font size. FORMAT: `(english-size . chinese-size)' or `english-font'")

;;;###autoload
(defvar lye-font-size-pair nil
  "The font size pair for (englih . chinese)")

;;; Zoom size corresponding to the pixel, point size
;; 字号        : 初号, 小初, 小一, 小二, 三号, 四号, 小四, 五号
;; 像素(Pixel) : 56px, 40px, 32px, 24px, 21px, 19px, 16px, 14px
;; 磅值(Point) : 42pt, 30pt, 24pt, 18pt, 15pt, 14pt, 12pt, 10.5pt

;;;###autoload
(defvar lye-font-size-pair-list nil "Grow and shrink the font list.")

;;;###autoload
(defun lye-font/exist-p! (fontname)
  "Test the font is exist or not."
  (if (find-font (font-spec :family fontname)) t nil))

(defun lye-font/size-pair-list-initialize! (english-font &optional use-pixel-p)
  "Initialize the list of setup font size."
  (cond
   ((string= english-font "Fantasque Sans Mono")
    (setq lye-font-size-pair-list
          (if use-pixel-p
              '((14 . 14) (16 . 16) (19 . 19) (21 . 21)
                (24 . 24) (32 . 32) (40 . 40) (56 . 56))
            '((10.5 . 10.5) (12.0 . 12.0) (14.0 . 14.0) (15.0 . 15.0)
              (18.0 . 18.0) (24.0 . 24.0) (30.0 . 30.0) (42.0 . 42.0)))))
   (t
    (setq lye-font-size-pair-list
          (if use-pixel-p
              '((14 . 16) (16 . 20) (19 . 22) (21 . 26)
                (24 . 28) (32 . 28) (40 . 48) (48 . 56)))))))

;;;###autoload
(defun lye-font/setup-size-pair+ ()
  "Gets font size."
  (unless lye-font-size-pair
    (setq lye-font-size-pair
          (if (listp lye-font-default-size)
              lye-font-default-size
            (assoc lye-font-default-size lye-font-size-pair-list)))))

;;;###autoload
(defun lye-font/setup-monospace-font! (font &optinal size)
  "Set the monospace font size when mixed CJK and English words."
  (let* ((eng-font (if (listp font) (car font) font))
         (cjk-font (if (listp font) (cdr font) nil)))
    ))
