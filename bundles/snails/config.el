;;; bundles/snails/config.el.el -*- lexical-binding: t -*-
;;; snails-backend-buffer-blacklist
(with-eval-after-load 'snails
  (if (fboundp 'fuz-build-and-load-dymod)
      (progn
        (lib-load-absolute 'bundles/common/fuz-core t t)
        (setq snails-fuz-library-load-status "load"))
    (setq snails-fuz-library-load-status "unload"))
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

;; selectrum
;; (setq prescient-save-file (lib-f-join lye-emacs-cache-dir "var/prescient-save.el"))
;; (add-hook! 'after-init-hook
;;   (selectrum-mode +1)
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))
