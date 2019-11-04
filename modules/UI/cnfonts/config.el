;;; modules/UI/cnfonts/config.el -*- lexical-binding: t -*-

(use-package cnfonts
  :init
  (setq cnfonts-directory (expand-file-name "cnfonts" lye-emacs-cache-dir))
  :bind (("C-, u f" . cnfonts-ui)))
