;;; bundles/lsp/package.el.el -*- lexical-binding: t -*-

(pcase lye-use-lsp-manager
  ('eglot (package! 'eglot :commands eglot-ensure))
  ('lsp-mode
   (package! 'lsp-mode :commands lsp)
   (package! 'lsp-ui)
   ;; (package! 'lsp-ivy)
   ;; (package! 'dap-mode)
   (package! 'company-lsp)))
