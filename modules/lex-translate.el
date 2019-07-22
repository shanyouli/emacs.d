;;; lex-translate.el --- Translate Tools -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (sdcv youdao-dictionary)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: translate


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

;; Translation tool settings

;;; Code:
(defvar one-key-menu-translate-alist nil
  "The `one-key' menu list for TRANSLATATIONS")

(cond
 ((string= lye-enable-sdcv-or-youdao "sdcv")
  (require 'sdcv)
  (require 'posframe)
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

  ;;Set shortcuts
  (setq one-key-menu-translate-alist
        '((("s" . "Search by translation") . sdcv-search-input+)
          (("d" . "Search by translation, Displayed in other frame") . sdcv-search-input)
          (("p" . "Point by translation") . sdcv-search-pointer+)
          (("o" . "Search by translation, Displayed in another frame") . sdcv-search-pointer)))
  (defun one-key-menu-translate ()
    "The `one-key' menu for TRANSLATATIONS"
    (interactive)
    (one-key-menu "TRANSLATATIONS" one-key-menu-translate-alist t)))

 ((string= lye-enable-sdcv-or-youdao "youdao")
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file
        (concat lye-emacs-cache-dir "youdaohs"))
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

;;Set shortcuts
  (setq one-key-menu-translate-alist
        '((("s" . "Search by translation") . youdao-dictionary-search-from-input)
          (("p" . "Point by translation") . youdao-dictionary-search-at-point-tooltip)
          (("o" . "Search by translation, Displayed in another frame") . youdao-dictionary-search)))
  (defun one-key-menu-translate ()
    "The `one-key' menu for TRANSLATATIONS"
    (interactive)
    (one-key-menu "TRANSLATATIONS" one-key-menu-translate-alist t))))


;; (when (locate-library "avy")
;;         (defun lye/avy-youdao-dictionary ()
;;           (interactive)
;;           (save-excursion
;;             (call-interactively #'avy-goto-char)
;;             (if (display-graphic-p)
;;                 (youdao-dictionary-search-at-point-tooltip)
;;               (youdao-dictionary-search-at-point))))
;;         (bind-key "C-s y" #'lye/avy-youdao-dictionary))

(provide 'lex-translate)

;;; lex-translate.el ends here
