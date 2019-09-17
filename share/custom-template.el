;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq lye-full-name "user name")     ; User full name
;; (setq lye-mail-address "user@email.com") ; Email address
(setq lye-use-fuz-or-flx-in-ivy nil)
(setq lye-company-enable-yas t)         ; Whether to use the company to complete yas-snippet
(setq lye-enable-benchmark-p nil)       ; Enable initialization benchmark or not: t or nil
(setq lye-enable-sdcv-or-youdao 'sdcv)  ; Use sdcv or youdao dictionary
(setq lye-package-archives 'tuna)       ; Package repo: melpa, melpa-mirror, emacs-china, netease, tencent or tuna

;;; Fonts set all unicode characters
(when (display-graphic-p)
  ;; Specify font for all unicode characters
  (add-hook 'after-init-hook
            (lambda ()

              (catch 'loop
                (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
                  (when (member font (font-family-list))
                    (set-fontset-font t 'unicode font nil 'prepend)
                    (throw 'loop t)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
