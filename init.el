;;; Package --- Summary

;;; Commentary:

;;; Code:
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
(require 'init-elpa)

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
(require 'init-chinese)
(require 'init-magit)
;; Language
(require 'init-rust)
(require 'init-ahk)
(require 'init-scheme)

(require-package 'aurel)
(autoload 'aurel-package-info "aurel" nil t)
(autoload 'aurel-package-search "aurel" nil t)
(autoload 'aurel-package-search-by-name "aurel" nil t)
(autoload 'aurel-maintainer-search "aurel" nil t)
(autoload 'aurel-installed-packages "aurel" nil t)



(when (file-exists-p custom-file)
  (load custom-file))

(when (require 'time-date nil t)
  (message "Emacs startup time: %.3f seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)
;;; init.el ends here

