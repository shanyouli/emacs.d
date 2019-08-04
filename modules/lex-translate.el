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
 ((eq lye-enable-sdcv-or-youdao 'sdcv)
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

  ;; Set an alias for the function
  (defalias 'default-translate-input 'sdcv-search-input
    "Better create shortcut groups,`sdcv/youdao-dictionary'")

  (defalias 'default-translate-point 'sdcv-search-pointer
    "Better create shortcut groups,`sdcv/youdao-dictionary'")

  (defalias 'default-translate-point+ 'sdcv-search-pointer+
    "Better create shortcut groups,`sdcv/youdao-dictionary'"))

 ((eq lye-enable-sdcv-or-youdao 'youdao)

  (require-package 'youdao-dictionary)
  (require 'youdao-dictionary)

  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file
        (concat lye-emacs-cache-dir "youdaohs"))

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; Set an alisas for the functions
  (defalias 'default-translate-input 'youdao-dictionary-search-from-input
    "Better create shortcut groups,`sdcv/youdao-dictionary'")

  (defalias 'default-translate-point 'youdao-dictionary-search-at-point
    "Better create shortcut groups,`sdcv/youdao-dictionary'")

  (defalias 'default-translate-point+ 'youdao-dictionary-search-at-point-tooltip
    "Better create shortcut groups,`sdcv/youdao-dictionary'")))

(when (locate-library "avy")
  (defun default-translate-use-avy ()
    (interactive)
    (save-excursion
      (call-interactively #'avy-goto-char)
      (if (display-graphic-p)
          (default-translate-point+)
        (default-translate-point)))))


(defhydra hydra-translate-menu (:exit t)
  "Translate Menu"
  ("d"  default-translate-input "Search by Input")
  ("t" default-translate-point+ "Search by Point+")
  ("p" default-translate-point "Search by Point")
  ("SPC" (lambda ()
           (interactive)
           (if (fboundp 'default-translate-use-avy) (default-translate-use-avy)))
   "Search by Goto a char")
  ("s" (lambda ()
         (interactive)
         (if (fboundp 'sdcv-search-input+) (sdcv-search-input+)))
   "Search by Input+"))

(provide 'lex-translate)

;;; lex-translate.el ends here
