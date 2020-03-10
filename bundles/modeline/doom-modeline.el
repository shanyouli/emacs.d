;;; bundles/ui/doom-modeline.el.el -*- lexical-binding: t -*-


(unless after-init-time
  (setq doom-modeline--old-format mode-line-format)
  (setq-default mode-line-format nil))

(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-color-icon t
      doom-modeline-minor-modes nil
      doom-modeline-mu4e nil
      doom-modeline-height 10
      doom-modeline-bar-width 2
      doom-modeline-buffer-file-name-style 'truncate-upto-root)

(add-hook! 'after-init-hook (doom-modeline-mode +1))
