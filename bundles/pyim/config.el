;;; bundles/pyim/config.el -*- lexical-binding: t -*-

(require 'pyim)

;; using pyim-dregcache, not use pyim-dhashcache
(setq pyim-dcache-backend 'pyim-dregcache)
;; Using Emacs-async, Emacs thread is nore stagnation than asynchronuous
(setq pyim-prefer-emacs-thread nil)
(setq pyim-dcache-directory (lib-f-join lye-emacs-cache-dir "pyim/dcache"))
(setq default-input-method "pyim")

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
    ('librime (lib-load-relative "liberime" t t)
              (pyim-liberime-enable))))
