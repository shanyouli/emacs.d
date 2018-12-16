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
(quelpa '(insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name"))
(require 'json)
(use-package insert-translated-name
  :commands (insert-translated-name-insert)
  :bind ("C-c t" . insert-translated-name-insert))

;; Configure Chinese input method
(use-package pyim
  :diminish pyim-isearch-mode
  :bind* (("M-j" . pyim-convert-code-at-point))
  :config
  ;; Activate the Pinyin of basedict
  (use-package pyim-basedict
    :config
    (eval-after-load 'pyim
      (pyim-basedict-enable)))
  ;; Set pyim as the default input method
  (setq default-input-method "pyim")
  ;;Use Emacs thread to generate dcache
  (setq pyim-dcache-prefer-emacs-thread t)
  (setq pyim-dcache-directory (concat lye-emacs-temporal-dir "pyim/dcache"))
  ;; Use full spell
  (setq pyim-default-scheme 'quanpin)
  ;; Probe setting
  (setq-default pyim-english-input-switch-functions
        '(pyim-probe-dynamic-english
          pyim-probe-isearch-mode
          pyim-probe-program-mode
          pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
        '(pyim-probe-punctuation-after-punctuation
          pyim-probe-punctuation-line-beginning))

  ;;Turn on pinyin search
  (pyim-isearch-mode 1)
  ;; Set 5 candidate words
  (setq pyim-page-length 5)
  ;; Set the way the word box is drawn
  (if (and (display-graphic-p)
       (>= emacs-major-version 26))
      (use-package posframe
    :config (setq pyim-page-tooltip 'posframe))
    (setq pyim-page-tooltip 'popup))

  ;; Cancel CHinese completion
  (if (featurep 'company)
      (with-eval-after-load 'company
    (defun lye/company-dabbrev-prefix (orig-fun)
      "Cancel Chinese completion."
      (let ((string (pyim-char-before-to-string 0)))
        (if (pyim-string-match-p "\\cc" string)
        nil
          (funcall orig-fun))))
    (advice-add 'company-dabbrev-prefix
            :around #'lye/company-dabbrev-prefix)))

  ;; ivy-regex-plus
  (when  (featurep 'ivy)
    (with-eval-after-load 'ivy
      (defun lye/ivy-cregexp (str)
    (concat
     (ivy--regex-plus str)
     "\\|"
     (pyim-cregexp-build str)))
      (setq ivy-re-builders-alist
        '((t . lye/ivy-cregexp))))))

(defun lye/toggle-input-method ()
  "Use pyim input method."
  (interactive)
  (unless (featurep 'pyim)
    (require 'pyim))
  (toggle-input-method))
(global-set-key (kbd "C-\\") 'lye/toggle-input-method)

;; Prompt English words when writing English
(quelpa '(company-english-helper :fetcher github :repo "manateelazycat/company-english-helper"))
(use-package company-english-helper
  :ensure nil
  :commands(toggle-company-english-helper))

;;translate Chinese to English, or translate English to Chinese
(if (eq (shell-command "type sdcv 2>&1 >/dev/null") 0)
    (progn
      (message "You Installed sdcv in the computer!")
      (quelpa '(sdcv :fetcher github :repo "manateelazycat/sdcv"))
      (use-package sdcv
        :ensure nil
        :commands (sdcv-search-pointer+ sdcv-search-pointer sdcv-search-input sdcv-search-input+)
        :bind (("C-c y" . sdcv-search-pointer+)
               ("C-c Y" . sdcv-search-input+))
        :config
                                        ; sdcv need posframe
        (unless (featurep 'posframe)
          (use-package posframe))
        ;; (setq sdcv-say-word-p t) ;say word after translation
        (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic") ; setup dictionary list for simple search
        (setq sdcv-dictionary-simple-list ; setup dictionary list for simple search
              '("KDic11万英汉词典"
                "懒虫简明英汉词典"
                "懒虫简明汉英词典"))
        (setq sdcv-dictionary-complete-list ; setup dictionary list for complete search
              '(
                "KDic11万英汉词典"
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
                "CDICT5英汉辞典"
                ))))
  (progn
    (message "Not Installed sdcv, Using YouDao dictionary")
    (use-package youdao-dictionary
      :bind (("C-c y" . youdao-dictionary-search-at-point)
             ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
      :config
      ;; Cache documents
      (setq url-automatic-caching t)
      ;; Set file path for saving search history
      (setq youdao-dictionary-search-history-file (concat lye-emacs-temporal-dir "youdaohs"))
      ;; Enable Chinese word segmentation support (支持中文分词)
      (setq youdao-dictionary-use-chinese-word-segmentation t))))




(provide 'init-chinese)
;;; init-chinese.el ends here
