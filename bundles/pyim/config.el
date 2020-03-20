;;; bundles/pyim/config.el -*- lexical-binding: t -*-

;; using pyim-dregcache, not use pyim-dhashcache
(setq pyim-dcache-backend 'pyim-dregcache)
;; Using Emacs-async, Emacs thread is nore stagnation than asynchronuous
(setq pyim-prefer-emacs-thread nil)
(setq pyim-dcache-directory (lib-f-join lye-emacs-cache-dir "pyim/dcache"))
(setq default-input-method "pyim")
(setq-default pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
(setq-default pyim-title "ㄓ")
(with-eval-after-load 'pyim
  ;; Fuzzy pinyin
  ;; (setq pyim-fuzzy-pinyin-alist
  ;;       '(("en" "eng") ("in" "ing") ("l" "n") ("z" "zh") ("c" "ch")
  ;;         ("s" "sh") ("an" "ang")))
  ;; Set 9 Candidate words
  (setq pyim-page-length 9)
  (if (and (display-graphic-p) (require 'posframe nil t))
      (setq pyim-page-tooltip 'posframe
            pyim-posframe-min-width 0)
    (setq pyim-page-tooltip 'minibuffer
          pyim-page-style 'one-line))
  (setq pyim-default-scheme 'quanpin)
  (pcase lye-use-pyim-dictionary
    ('base (pyim-basedict-enable))
    ('big (pyim-bigdict-enable))
    ('librime (lye-load! "liberime" nil t t)
              (pyim-liberime-enable))))

(defun bundle-pyim-punctuation-toggle ()
  (interactive)
  (setq-default pyim-punctuation-translate-p
                (pcase (car pyim-punctuation-translate-p)
                  ;; 更改为中文全角，英文半角
                  ('no  '(auto yes no))
                  ;; 更改为全部半角
                  ('auto '(no yes auto))
                  ;; 全部全角改为全部半角
                  ('yes '(no yes auto))))
  (when (and current-input-method (string= current-input-method "pyim"))
    (toggle-input-method)
    (set-input-method "pyim")))

(defun lye//require-pyim (&rest _)
  "Toggle-input method"
  (unless (featurep 'pyim) (require 'pyim nil t)))

(defun lye/toggle-input-method ()
  "Toggle-input method"
  (interactive)
  (lye//require-pyim)
  (toggle-input-method))
