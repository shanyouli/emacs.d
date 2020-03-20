;;; bundles/flycheck/config.el.el -*- lexical-binding: t -*-

;;;; Code
;; (add-hook! 'after-init-hook (global-flycheck-mode +1))
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (custom-set-faces
   '(flycheck-posframe-border-face ((t (:inherit default)))))
  (add-hook! 'flycheck-mode-hook (flycheck-posframe-mode +1))
  (setq flycheck-posframe-border-width 1
        flycheck-posframe-inhibit-functions
        '((lambda (&rest _) (bound-and-true-p company-backend)))))

(add-hook! 'prog-mode-hook
    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
      (flycheck-mode +1)))
