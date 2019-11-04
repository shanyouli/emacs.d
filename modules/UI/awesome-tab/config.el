;;; modules/UI/awesome-tab/config.el -*- lexical-binding: t -*-

(use-package awesome-tab
  :if (display-graphic-p)
  :commands (awesome-tab-mode)
  :hook ((after-init . awesome-tab-mode))
  :config
  ;; awesome-tab style: slant, wave,alternate,bar,box,chamfer,rounded,zigzag
  (setq awesome-tab-style 'zigzag)
  (setq awesome-tab-hide-tab-function 'lye/awesome-tab-hide-tab+)
  (if (boundp 'after-load-theme-hook)
      (add-hook 'after-load-theme-hook #'lye/refresh-awesome-tab-mode+)))
