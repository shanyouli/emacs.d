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

;;; Start pyim function
(defvar pyim-load-liberime-or-pyim-bigdict-p t
  "If pyim-load-liberime-or-pyim-bigdict-p is t and the liberime package exists,
use liberime as the back end of the pyim input method.
Conversely use pyim-bigdict as the lexical backend for the pyim input method.")

(defvar previous-themes-is nil
  "A variable determines if the Theme changes.")

(when (locate-library "posframe")
  (require 'posframe))

;; pyim-bigdict
(defun pyim-bigdict-enable ()
  "Add bigdict to pyim"
  (interactive)
  (let* ((file lye-emacs-pyim-big-file)
         (newfile (concat lye-emacs-cache-dir "pyim/pyim-bigdict.pyim")))
    (unless (file-exists-p newfile)
      (ungzip-the-file file newfile))
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
  ;; Set 9 candidate words
  (setq pyim-page-length 9)
  (if (and (display-graphic-p) (featurep 'posframe))
        (setq pyim-page-tooltip 'posframe
              pyim-posfram-min-width 0)
    (setq pyim-page-tooltip 'minibuffer)))

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

  (setq default-input-method "pyim") ;set pyim as the default input method

  (unless (featurep 'liberime)
    (lye/modules-require 'lex-liberime)
    (setq liberime-user-dir (expand-file-name "pyim/rime" lye-emacs-cache-dir))
    (liberime-initialize))

  (unless (featurep 'liberime)
    (pyim-load-bigdict)))

(defun changes-pyim-page-color-from-theme ()
  "Set different color schemes for `pyim-page' according to different topics."
  (let ((current-themes (car custom-enabled-themes)))
    (unless (and previous-themes-is
                 (string= previous-themes-is current-themes))
      (set-face-attribute 'pyim-page nil
                          :background (face-foreground 'default)
                          :foreground (face-background 'default))
      (setq previous-themes-is current-themes))))

(defun toggle-default-pyim-input-method ()
  "Use pyim as the default output."
  (interactive)
  ;; Set the color of pyim-bage
  (changes-pyim-page-color-from-theme)
  (cond
   (pyim-load-liberime-or-pyim-bigdict-p
        (pyim-load-liberime-or-pyim-bigdict)
        (setq pyim-load-liberime-or-pyim-bigdict-p nil)
        (set-input-method "pyim"))
   ((not default-input-method)
    (setq default-input-method "pyim")
    (pyim-load-bigdict)
    (set-input-method "pyim"))
   (t
    (toggle-input-method))))

(defun lye/toggle-pyim-punctuation-translate()
  "Half-width punctuation and full-width punctuation conversion."
  (interactive)
  (if (string= "no" (car pyim-punctuation-translate-p))
      (setq-default pyim-punctuation-translate-p '(auto yes no))
    (setq-default pyim-punctuation-translate-p '(no yes auto)))
  (pyim-restart-1 t t))

(provide 'lex-pyim)

;;; lex-pyim.el ends here
