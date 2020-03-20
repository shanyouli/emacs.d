;;; bundles/snails/config.el.el -*- lexical-binding: t -*-

;; Not use exec-path-from-shell-initialize
(setq snails-use-exec-path-from-shell nil)
(with-eval-after-load 'snails
  (if (fboundp 'fuz-build-and-load-dymod)
      (progn
        (lye-load! 'bundles/common/fuz-core nil t t)
        (setq snails-fuz-library-load-status "load"))
    (setq snails-fuz-library-load-status "unload"))
  ;;; snails-backend-buffer-blacklist
  (dolist (buf (list
                " *which-key*"
                " *straight-process*"
                "*straight-process*"
                "*One-Key*"
                "*Flycheck**"
                "*flycheck-posframe-buffer*"
                " *flycheck-posframe-buffer*"
                " *company-posframe-buffer*"
                "*company"
                " *company"
                "*esup"
                " *pyim"
                " *server"
                " *sdcv"
                " *diff-hl*"
                " *snails"))
    (push buf snails-backend-buffer-blacklist)))

;;; snails-backend-themes
(defun snails-load-theme ()
  "Loading a theme use `snails'"
  (interactive)
  (require 'snails-backend-themes)
  (snails '(snails-backend-themes)))
