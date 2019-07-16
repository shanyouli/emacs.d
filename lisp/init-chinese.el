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
    :commands (sdcv-search-pointer+
               sdcv-search-pointer
               sdcv-search-input
               sdcv-search-input+)
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
    (setq youdao-dictionary-search-history-file
          (concat lye-emacs-cache-dir "youdaohs"))
    ;; Enable Chinese word segmentation support (支持中文分词)
    (setq youdao-dictionary-use-chinese-word-segmentation t)))

(if (or (and (boundp system/windows) system/windows)
        (not (executable-find "sdcv")))
    (progn
      (lye/use-youdao-dic)
      (when (locate-library "avy")
        (defun lye/avy-youdao-dictionary ()
          (interactive)
          (save-excursion
            (call-interactively #'avy-goto-char)
            (if (display-graphic-p)
                (youdao-dictionary-search-at-point-tooltip)
              (youdao-dictionary-search-at-point))))
        (bind-key "C-s y" #'lye/avy-youdao-dictionary)))
  (lye/use-sdcv))


;;; pyim -- chinese input method
(use-package pyim
  :demand t
  :defer 0.3
  :ensure nil
  :bind ("<f9>" . toggle-input-method)
  :preface
  (defun pyim-bigdict-enable ()
    "Add bigdict to pyim"
    (interactive)
    (let* ((file (concat
                  (file-name-as-directory user-emacs-directory)
                  (file-name-as-directory "pyim-dict")
                  "pyim-bigdict.pyim.gz")))
      (when (file-exists-p file)
        (if (featurep 'pyim)
            (pyim-extra-dicts-add-dict
             `(:name "Bigdict-elpa"
                     :file ,file
                     :coding utf-8-unix
                     :dict-type pinyin-dict
                     :elpa t))
          (message "Pyim didn't pretend, pyim-bigdict failed to start.")))))
  :init
  ;; Using pyim-dregcache. not use pyim-dhashcache
  (setq pyim-dcache-backend 'pyim-dregcache)
  ;; Set pyim as the default input method
  (setq default-input-method "pyim")

  ;; No Chinese company
  (with-eval-after-load 'company
    (defun lye/company-dabbrev--prefix (orig-fun)
      "取消中文补全"
      (let ((string (pyim-char-before-to-string 0)))
        (if (pyim-string-match-p "\\cc" string)
            nil
          (funcall orig-fun))))
    (advice-add 'company-dabbrev--prefix
                :around #'lye/company-dabbrev--prefix))
  :config
  (pyim-bigdict-enable)
  ;; Use Emacs async to dcache, Emacs thread is more stagnation than asynchronous
  (setq pyim-prefer-emacs-thread nil)
  (setq pyim-dcache-directory (concat lye-emacs-cache-dir "pyim/dcache"))
  ;; Use full spell
  (setq pyim-default-scheme 'quanpin)

  ;; Set the way the word box is drawn
  (use-package posframe
    :if (and (display-graphic-p) (>= emacs-major-version 26))
    :config
    (setq pyim-page-tooltip 'posframe)
    (setq pyim-posframe-min-width 0))

  (unless (string= pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  ;; Fuzzy pinyin
  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng") ("in" "ing") ("l" "n") ("z" "zh") ("c" "ch")
          ("s" "sh") ("an" "ang")))

  ;; Set 9 candidate words
  (setq pyim-page-length 9)

  (setq-default pyim-punctuation-translate-p '(no yes auto))
  ;;(setq-default pyim-punctuation-translate-p '(auto yes no ))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode))

  ;; Half-width punctuation and full-width punctuation conversion
  (defun lye/toggle-pyim-punctuation-translate()
    (interactive)
    (toggle-input-method)
    (if (string= "no" (car pyim-punctuation-translate-p))
        (setq-default pyim-punctuation-translate-p '(auto yes no))
      (setq-default pyim-punctuation-translate-p '(no yes auto)))
    (toggle-input-method))
  (global-set-key (kbd "C-<f9>") #'lye/toggle-pyim-punctuation-translate))

(provide 'init-chinese)
;;; init-chinese.el ends here
