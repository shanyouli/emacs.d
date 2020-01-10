;;; bundles/fcitx/config.el -*- lexical-binding: t -*-

(require 'fcitx)

;; (fcitx-prefix-keys-add "C-h")
(fcitx-aggressive-setup)

(with-eval-after-load 'pyim
  (add-hook! 'pyim-active-hook #'fcitx--deactivate))
