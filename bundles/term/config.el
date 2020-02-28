;;; bundles/term/config.el -*- lexical-binding: t -*-

(when (eq lye-use-term-package 'vterm)
  (lib-load-relative "vterm" t t))

(setq shell-pop-window-size 40
      shell-pop-shell-type
      (pcase lye-use-term-package
        ('vterm '("vterm" "*vterm*" #'vterm))
        ('multi-term '("multi-term" "*Multi-TERM*" #'multi-term))
        ('eshell '("eshell" "*Eshell*" (lambda () (eshell))))))

;; (require 'shell-pop)
