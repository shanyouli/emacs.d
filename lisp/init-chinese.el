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
  :quelpa (insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name"))
(require 'insert-translated-name)
(global-set-key (kbd "M-t") 'insert-translated-name-insert)

;; Configure Chinese input method
(use-package pyim
  :bind* (("M-j" . pyim-convert-code-at-point))
  :diminish pyim-isearch-mode
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
  (if (featurep 'ivy)
      (with-eval-after-load 'ivy
	(defun lye/ivy-cregexp (str)
	  (concat
	   (ivy--regex-plus str)
	   "\\|"
	   (pyim-cregexp-build str)))
	(setq ivy-re-builders-alist
	      '((t . lye/ivy-cregexp)))))
  )

(defun lye/toggle-input-method ()
  "Use pyim input method."
  (interactive)
  (unless (featurep 'pyim)
    (require 'pyim))
  (toggle-input-method))
(global-set-key (kbd "C-\\") 'lye/toggle-input-method)

(quelpa '(company-english-helper :fetcher github :repo manateelazycat/company-english-helper))
(require 'company-english-helper)


(provide 'init-chinese)
;;; init-chinese.el ends here
