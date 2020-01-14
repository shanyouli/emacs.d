;;; bundles/lsp/config.el.el -*- lexical-binding: t -*-

;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (unless filename
               (user-error "LSP:: specify `:file' property to enable."))

             (setq buffer-file-name filename)
             (pcase lye-use-lsp-manager
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp-deferred)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp-deferred))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      lye-use-lsp-manager (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list "sh")
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang)))

(pcase lye-use-lsp-manager
  ('eglot (add-hook! 'prog-mode-hook (eglot-ensure)))
  ('lsp-mode (lib-load-relative "lsp-mode" t t)))
