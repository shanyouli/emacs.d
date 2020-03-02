;;; bundles/mode/config.el -*- lexical-binding: t -*-

;; Plantuml-mode
(setq plantuml-default-exec-mode 'jar)
(with-eval-after-load 'plantuml-mode
  (setq plantuml-jar-path
        (lib-f-join (getenv "HOME") ".local/share/jar/plantuml/plantuml.jar"))
  (unless (file-exists-p plantuml-jar-path)
    (lib-f-make-parent-dir plantuml-jar-path)
    (plantuml-download-jar)))

;; Only suitable for Windows Languages-Packages major-mode
(when IS-WINDOWS
  (setq explicit-shell-file-name
        "C:\\Windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
  ;; interactive, but no command prompt
  (setq explicit-powershell.exe-args '("-Command" "-")))


;; SH-MODE
(add-hook! 'sh-mode-hook
  ;; Determine if the shell using the running script is zsh or bash
  (let ((f (if buffer-file-name
               (file-name-nondirectory buffer-file-name)
             nil)))
    (if (and f (or (string-match "^\\.zsh\\(\\rc\\|\\env\\|func\\)$" f)
                   (string-match "\\.zsh$" f)
                   (string-match "^\\.zlogin$" f)))
        (sh-set-shell "zsh")
      (sh-set-shell "bash")))
  ;; Run LSP
  (when (and (string= sh-shell "bash")
             (executable-find "bash-language-server")
             (bundle-active-p 'lsp)
             (eq lye-use-lsp-manager 'lsp-mode))
    (lsp)))

;; LUA-MODE
(with-eval-after-load 'lua-mode
  (setq lua-indent-level 4
        lua-indent-string-contents nil)
  ;; lsp
  (require 'lsp-lua-emmy)
  (setq lsp-lua-emmy-jar-path
        (expand-file-name "~/.local/share/jar/emmylua/EmmyLua-LS-all.jar")))
(add-hook! 'lua-mode-hook
    (lsp)
  (setq-local company-backends
              (cons '(company-lsp company-lua company-yasnippet)
                    company-backends)))

;; PYthon
(with-eval-after-load 'python
  (require 'pyenv-mode-auto nil t))
(add-hook! 'python-mode-hook :if (require 'yapfify nil t)
  (yapf-mode +1))
(add-hook! 'python-mode-hook
  :if (executable-find "pyls")
  (lsp))
