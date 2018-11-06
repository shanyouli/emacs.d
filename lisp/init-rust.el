;;; init-rust.el --- Initialize Rust configurations

;; about racer Install
;; You maybe install rust-nightly
;; @see https://github.com/racer-rust/emacs-racer
;; Installation 1--3
;; rustup toolchain add nightly
;; rustup component add rust-src
;; cargo +nightly install racer

;;; Commentary:
;; rust languager

;;; code:

(eval-when-compile
  (require 'init-elpa)
  (require 'init-utils))

(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode))
  (when (maybe-require-package 'company)
    (add-hook 'racer-mode-hook #'company-mode)))

(when (maybe-require-package 'flycheck-rust)
  (after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)
;;; init-rust.el ends here
