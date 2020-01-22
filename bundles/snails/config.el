;;; bundles/snails/config.el.el -*- lexical-binding: t -*-


(require 'snails)

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
              " *diff-hl*"))
  (push buf snails-backend-buffer-blacklist))

;;; snails-backend-themes
(defun snails-load-theme ()
  "Loading a theme use `snails'"
  (interactive)
  (require 'snails-backend-themes)
  (snails '(snails-backend-themes)))

(with-eval-after-load 'snails
  ;; UP, Down, Left, Right key bind
  (define-key snails-mode-map (kbd "<up>") 'snails-select-prev-item)
  (define-key snails-mode-map (kbd "<down>") 'snails-select-next-item)
  (define-key snails-mode-map (kbd "<left>") 'snails-select-prev-backend)
  (define-key snails-mode-map (kbd "<right>") 'snails-select-next-backend))
