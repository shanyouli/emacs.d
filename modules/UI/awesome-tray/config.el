;;; modules/UI/awesome-tray/config.el -*- lexical-binding: t -*-

(use-package awesome-tray
  :commands (awesome-tray-mode awesome-tray-enable)
  :hook (after-init . awesome-tray-initialize+)
  :config
  (push '("pyim" . (awesome-tray-module-pyim-info awesome-tray-module-pyim-face))
        awesome-tray-module-alist)

  (setq awesome-tray-active-modules
        '("pyim" "location"  "parent-dir"  "mode-name" "awesome-tab" "date"))
  (advice-add #'awesome-tray-enable :after
              (lambda () (setq-default mode-line-format '(" ")))))
