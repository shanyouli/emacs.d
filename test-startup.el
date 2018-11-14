;;; test-startup --- test Emacs startup time

;;; Commentary:
;; Usage:
;; emacs -Q ~/.emacs.d/test-startup.el

;;; Code:
(defvar emacs-startup-time(current-time))

(add-hook 'after-init-hook
          (lambda ()
            (message (format "Init completed in %.6fs\n\n" ( - (float-time (current-time) (float-time emacs-startup-hook)))))
            ))
(provide 'test-startup)
;;; test-startup.el ends here
