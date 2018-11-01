;;; Start

(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 192 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold (* 32 1024 1024)))))

(setq emacs-load-start-time (current-time))
;;-----------------------------------------------------------------------------
;; Bootstrap config
;;-----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
(require 'init-package)

(require-package 'diminish)
(require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-theme)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-flycheck)
(require 'init-ivy)
(require 'init-company)
(require 'init-paredit)
(require 'init-which-key)
(require 'init-fonts)
(require 'init-edit-utils)

;; Language
(require 'init-rust)
(require 'init-ahk)

(when (require 'time-date nil t)
  (message "Emacs startup time: %.3f seconds."
           (time-to-seconds (time-since emacs-load-start-time))))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;;end

