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

;;; pyim -- chinese input method
(use-package pyim
  :demand t
;;  :defer 0.3
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
  ;; using pyim-dregcache. not use pyim-dhashcache
  (setq pyim-dcache-backend 'pyim-dregcache)
  ;; set pyim as the default input method
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

(defun lye/use-liberime ()
  (interactive)
  (when (locate-library "liberime-config")
    (require 'liberime-config)
    (liberime-select-schema "luna_pinyin_simp")
    (setq pyim-default-scheme 'rime-quanpin)
    (setq toggle-input-method "pyim")
    (set-input-method "pyim")))

(provide 'init-chinese)
;;; init-chinese.el ends here
