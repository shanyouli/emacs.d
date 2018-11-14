;;; init --- Emacs configuations.

;;; Commentary:

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

(defvar emacs-load-start-time
  "Getting Emacs startup time"
  nil)
(setq emacs-load-start-time (current-time))

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 192 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 32 1024 1024))
            (add-hook 'focus-out-hook 'garbage-collect)))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load 'custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Custom some used function
(require 'init-utils)

;; Packages
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



(when (require 'time-date nil t)
  (message "Emacs startup time: %.3f seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)
;;; init.el ends here

