;;; init-chinese.el --- Initialize Chinese Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>

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

;; ZH Automatically translated as EN
(use-package insert-translated-name
  :ensure nil
  :commands (insert-translated-name-insert-original-translation)
  :init (setq-default insert-translated-name-default-style "origin")
  :bind ("C-c t" . insert-translated-name-insert-original-translation))
;; Prompt English words when writing English
(use-package company-english-helper
  :ensure nil
  :commands(toggle-company-english-helper))

;;translate Chinese to English, or translate English to Chinese
(defun lye/use-sdcv ()
  "Installing sdcv and using sdcv package"
  (use-package sdcv
    :ensure nil
    :commands (sdcv-search-pointer+ sdcv-search-pointer sdcv-search-input sdcv-search-input+)
    :bind (("C-c y" . sdcv-search-pointer+)
               ("C-c Y" . sdcv-search-input+))
    :config
    (unless (featurep 'posframe) ; sdcv need posframe
      (use-package posframe))
    ;; (setq sdcv-say-word-p t) ;say word after translation
    (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic") ; setup dictionary list for simple search
    (setq sdcv-dictionary-simple-list ; setup dictionary list for simple search
          '("KDic11万英汉词典"
            "懒虫简明英汉词典"
            "懒虫简明汉英词典"))
    (setq sdcv-dictionary-complete-list ; setup dictionary list for complete search
          '("KDic11万英汉词典"
            "懒虫简明英汉词典"
            "懒虫简明汉英词典"
            "21世纪英汉汉英双向词典"
            "stardict1.3汉英词典"
            "新世纪汉英科技大词典"
            "牛津现代英汉双解词典"
            "XDICT汉英辞典"
            "XDICT英汉辞典"
            "朗道汉英字典5.0"
            "朗道英汉字典5.0"
            "quick_eng-zh_CN"
            "CDICT5英汉辞典")))
  )
(defun lye/use-youdao-dic ()
  "Installing and Using youdao-dictionary"
  (use-package youdao-dictionary
    :bind (("C-c Y" . youdao-dictionary-search-at-point)
           ("C-c y" . youdao-dictionary-search-at-point-tooltip))
    :config
    ;; Cache documents
    (setq url-automatic-caching t)
    ;; Set file path for saving search history
    (setq youdao-dictionary-search-history-file (concat lye-emacs-temporal-dir "youdaohs"))
    ;; Enable Chinese word segmentation support (支持中文分词)
    (setq youdao-dictionary-use-chinese-word-segmentation t)))

(cond
 ((or system/windows (not (executable-find "sdcv")))
  (lye/use-youdao-dic)
  (when (and (display-graphic-p) (featurep 'avy))
    ;;@https://emacs-china.org/t/topic/3676/4
    (defun lye/avy-youdao-dictionary ()
      (interactive)
      (save-excursion
        (call-interactively #'avy-goto-char)
        (youdao-dictionary-search-at-point-tooltip)))
    (bind-key "C-c y" #'lye/avy-youdao-dictionary)))
 (t
  (lye/use-sdcv)))

(provide 'init-chinese)
;;; init-chinese.el ends here
