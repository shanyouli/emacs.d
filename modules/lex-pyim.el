;;; lex-pyim.el --- PinYin Input Methods -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (pyim liberime)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: PinYin


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

;; PinYin Input Methods

;;; Code:
(require 'pyim)
(require 'async)
(when (and (>= emacs-major-version 26) (locate-library "posframe"))
  (require 'posframe))

;; pyim-bigdict
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

;; using pyim-dregcache, not use pyim-dhashcache
(setq pyim-dcache-backend 'pyim-dregcache)
;; Use Emacs async to dcache, Emacs thread is more stagnation than asynchronous
(setq pyim-prefer-emacs-thread nil)
(setq pyim-dcache-directory (concat lye-emacs-cache-dir "pyim/dcache"))

(with-eval-after-load 'pyim
  ;; Loading dictionary
  ;;  (pyim-bigdict-enable)
  ;; set pyim as the default input method
  (setq default-input-method "pyim")
  ;; Use full spell
  (setq pyim-default-scheme 'quanpin)
  ;; Fuzzy pinyin
  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng") ("in" "ing") ("l" "n") ("z" "zh") ("c" "ch")
          ("s" "sh") ("an" "ang")))

  (if (featurep 'posframe)
      (progn
        (setq pyim-page-tooltip 'posframe)
        (setq pyim-posfram-min-width 0))
    (setq pyim-page-tooltip 'popup))
  ;; Set 9 candidate words
  (setq pyim-page-length 9)
  )

(setq-default pyim-punctuation-translate-p '(no yes auto))
;;(setq-default pyim-punctuation-translate-p '(auto yes no ))
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-isearch-mode))

(defvar lye-enable-pyim-bigdict-p t)

(defun lye/toggle-pyim-input-method ()
  "Use pyim as the default output."
  (interactive)
  (when lye-enable-pyim-bigdict-p
    (pyim-bigdict-enable)
    (setq lye-enable-pyim-bigdict-p nil))
  (if toggle-input-method-active
      (progn
        (setq default-input-method "pyim")
        (set-input-method "pyim"))
    (toggle-input-method)))

;; Half-width punctuation and full-width punctuation conversion
(defun lye/toggle-pyim-punctuation-translate()
  (interactive)
  (toggle-input-method)
  (if (string= "no" (car pyim-punctuation-translate-p))
      (setq-default pyim-punctuation-translate-p '(auto yes no))
    (setq-default pyim-punctuation-translate-p '(no yes auto)))
  (toggle-input-method))

(defvar lye-liberime-share-dir "/usr/share/rime-data")
(defvar lye-liberime-user-dir (expand-file-name "pyim/rime" lye-emacs-cache-dir))
(defun lye/use-liberime ()
  (interactive)
  (if  (memq system-type '(gnu/linux darwin))
      (let* ((liberime--root (file-name-directory (locate-library "liberime-config")))
             (liberime--module (concat liberime--root "build/liberime" module-file-suffix)))
        (if (and liberime--root (file-exists-p liberime--module))
            (progn
              (load liberime--module)
              (liberime-start lye-liberime-share-dir lye-liberime-user-dir)
              (liberime-select-schema "luna_pinyin_simp")
              (setq pyim-default-scheme 'rime-quanpin)
              (setq lye-enable-pyim-bigdict-p nil)
              (set-input-method "pyim"))
          (message "Please compile liberime and use this function.")))
    (message "Limerime cannot be used on systems other than gnu/linux and Mac.")))

  ;; No Chinese company
;;  (with-eval-after-load 'company
    (defun lye/company-dabbrev--prefix (orig-fun)
      "取消中文补全"
      (let ((string (pyim-char-before-to-string 0)))
        (if (pyim-string-match-p "\\cc" string)
            nil
          (funcall orig-fun))))
    (advice-add 'company-dabbrev--prefix
                :around #'lye/company-dabbrev--prefix)
;;)

(provide 'lex-pyim)

;;; lex-pyim.el ends here
