
;;init-flycheck

;;; Code:
(require-package 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)

(setq flycheck-indication-mode 'right-fringe)
(setq flycheck-emacs-lisp-load-path 'inherit)


(provide 'init-flycheck)
