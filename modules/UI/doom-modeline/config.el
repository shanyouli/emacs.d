;;; module/UI/doom-modeline/config.el -*- lexical-binding: t -*-

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format '()))

  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil)

  (unless (get-buffer "*scratch*")
    (remove-hook 'after-change-major-mode-hook #'remove-scratch-buffer)
    (switch-to-buffer "*scratch*")))
