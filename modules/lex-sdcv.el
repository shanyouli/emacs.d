;;; lex-sdcv.el --- Init Sdcv -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (sdcv)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tranlations


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

;; sdcv

;;; Code:

(if (locate-library "posframe")
    (require 'posframe)
  (require-package 'posframe)
  (require 'posframe))

(require 'sdcv)

(set-face-attribute 'sdcv-tooltip-face nil
                    :foreground (face-background 'default)
                    :background (face-foreground 'default))

;; (setq sdcv-say-word-p t) ; say word after translation
(if (file-directory-p lye-sdcv-dictionary-data-dir)
    (setq sdcv-dictionary-data-dir lye-sdcv-dictionary-data-dir))
(setq sdcv-dictionary-simple-list ; setup dictionary list for simple search
      '("KDic11万英汉词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"))
(setq sdcv-dictionary-complete-list ; setup dictionary list for complete search
      '("KDic11万英汉词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "21世纪英汉汉英双向词典"
        "新世纪汉英科技大词典"
        "牛津现代英汉双解词典"
        "XDICT汉英辞典"
        "XDICT英汉辞典"
        "朗道汉英字典5.0"
        "朗道英汉字典5.0"
        "quick_eng-zh_CN"
        "CDICT5英汉辞典"))

(defun sdcv-search-at-point-tooltip ( &optional world)
  (interactive)
  (let ((word (sdcv-search-with-dictionary world sdcv-dictionary-simple-list)))
    (when word
      (require 'pos-tip)
      (pos-tip-show word nil nil nil 0))))

(defun sdcv-search-at-point++ ()
  (interactive)
  (cond
   ((and (display-graphic-p) (> emacs-major-version 25))
    (call-interactively #'sdcv-search-pointer+))
   ((display-graphic-p)
    (call-interactively #'sdcv-search-at-point-tooltip))
   (t
    (call-interactively #'sdcv-search-pointer))))

(provide 'lex-sdcv)

;;; lex-sdcv.el ends here
