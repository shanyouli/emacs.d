;;; bundles/lsp/lsp-mode.el.el -*- lexical-binding: t -*-

;; (add-hook! 'prog-mode-hook
;;     (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
;;       (lsp-deferred)))

(setq lsp-auto-guess-root nil        ; Detect project root
      lsp-keep-workspace-alive nil ; Auto-kill LSP server
      lsp-prefer-flymake nil       ; Use lsp-ui and flycheck
      flymake-fringe-indicator-position 'right-fringe
      lsp-session-file (concat lye-emacs-cache-dir "lsp-session"))

(with-eval-after-load 'lsp-mode
  ;; lsp-clients Configure LSP clients
  (add-hook! 'go-mode-hook
    "Format and add/delete imports."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  (unless (executable-find "rls")
    (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls")))

  ;; lsp-UI
  (custom-set-faces
   `(lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
   '(lsp-ui-sideline-code-action ((t (:inherit warning)))))

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))

        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t)

  (with-eval-after-load 'lsp-ui
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook! 'lye-load-theme-hook
    (setq lsp-ui-doc-border (face-foreground 'default))
    (set-face-background 'lsp-ui-doc-background
                         (face-background 'tooltip)))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defun my-lsp-ui-imenu-hide-mode-line ()
    "Hide the mode-line in lsp-ui-imenu."
    (setq mode-line-format nil))
  (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line))

  ;; Completion
  (setq company-lsp-cache-candidates 'auto))
