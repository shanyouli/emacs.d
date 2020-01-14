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
