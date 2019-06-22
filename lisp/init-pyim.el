;;; init-pyim.el --- Chinese Input Method            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:

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

;; Configure Chinese input method
(use-package pyim)
(require 'pyim)
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

(pyim-bigdict-enable)
;; Set pyim as the default input method
(setq default-input-method "pyim")

;; Use Emacs async to dcache, Emacs thread is more stagnation than asynchronous
(setq pyim-dcache-prefer-emacs-thread nil)
(setq pyim-dcache-directory (concat lye-emacs-cache-dir "pyim/dcache"))
;; Use full spell
(setq pyim-default-scheme 'quanpin)

;; Set the way the word box is drawn
(if (and (display-graphic-p)
         (>= emacs-major-version 26))
    (use-package posframe
      :config
      (setq pyim-page-tooltip 'posframe)
      (setq pyim-posframe-min-width 0))
  (setq pyim-page-tooltip 'popup))

;; Fuzzy pinyin
(setq pyim-fuzzy-pinyin-alist
      '(("en" "eng") ("in" "ing") ("l" "n") ("z" "zh") ("c" "ch")
        ("s" "sh") ("an" "ang")))

;; Set 9 candidate words
(setq pyim-page-length 9)

(setq-default pyim-punctuation-translate-p '(no yes auto))
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-isearch-mode))

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

;; Start Emacs automatically load the thesaurus
;; (add-hook 'emacs-startup-hook
;;           #'(lambda ()
;;               (with-temp-message
;;                   (with-current-buffer " *Minibuf-0*" (buffer-string))
;;                 (pyim-restart-1 t))))

(provide 'init-pyim)
;;; init-pyim.el ends here
