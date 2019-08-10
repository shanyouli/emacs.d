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

(when (and (>= emacs-major-version 26) (locate-library "posframe"))
  (require 'posframe))

;; pyim-bigdict
(defun pyim-bigdict-enable ()
  "Add bigdict to pyim"
  (interactive)
  (let* ((file lye-emacs-pyim-big-file)
         (newfile (concat lye-emacs-cache-dir "pyim/pyim-bigdict.pyim")))
    (unless (file-exists-p newfile)
      (lye/core-require 'core-funcs)
      (gzip-a-file file newfile))
    (when (file-exists-p newfile)
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "Bigdict-elpa"
             :file ,newfile
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
  (setq pyim-page-length 9))

(setq-default pyim-punctuation-translate-p '(no yes auto))
;;(setq-default pyim-punctuation-translate-p '(auto yes no ))
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-isearch-mode))

;; No Chinese company
(defun lye/company-dabbrev--prefix (orig-fun)
  "取消中文补全"
  (let ((string (pyim-char-before-to-string 0)))
    (if (pyim-string-match-p "\\cc" string)
        nil
      (funcall orig-fun))))
(if (featurep 'company)
    (advice-add 'company-dabbrev--prefix
                :around #'lye/company-dabbrev--prefix))

;; Use the cursor format to determine whether to use pyim. When using pyim,
;; the cursor is bar. When pyim is not applicable, the cursor is box.
(add-hook 'input-method-activate-hook  (lambda () (setq cursor-type 'bar)))
(add-hook 'input-method-deactivate-hook (lambda () (setq cursor-type t)))

;; use-pyim-bigdict-enable
(defun pyim-load-bigdict ()
  "Use pyim-bigdict as a full-fledged dictionary."
  (pyim-bigdict-enable) ; load dictionary
  ;; Use full spell
  (setq pyim-default-scheme 'quanpin))

(defun pyim-load-liberime-or-pyim-bigdict ()
  "If you can use liberime, use liberime.
If you can't use liberime, use pyim-bigdict."

  ;; set pyim as the default input method
  (setq default-input-method "pyim")

  (unless (featurep 'liberime)
    (lye/modules-require 'lex-liberime)
    (setq liberime-user-dir (expand-file-name "pyim/rime" lye-emacs-cache-dir))
    (liberime-initialize))

  (unless (featurep 'liberime)
    (pyim-load-bigdict)))

;;; Start pyim function
(defvar pyim-load-liberime-or-pyim-bigdict-p t)

(defun toggle-default-pyim-input-method ()
  "Use pyim as the default output."
  (interactive)
  (if pyim-load-liberime-or-pyim-bigdict-p
      (progn
        (pyim-load-liberime-or-pyim-bigdict)
        (setq pyim-load-liberime-or-pyim-bigdict-p nil)
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

(provide 'lex-pyim)

;;; lex-pyim.el ends here
