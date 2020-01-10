;;; bundles/fcitx/config.el -*- lexical-binding: t -*-

(when (lye-is-running-p "fcitx")
  (require 'fcitx)

  ;; (fcitx-prefix-keys-add "C-h")
  (fcitx-aggressive-setup)

  (with-eval-after-load 'pyim
    (add-hook! 'pyim-active-hook #'fcitx--deactivate)))
