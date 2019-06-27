;;; init-theme.el -- Initialize theme configuration -*- lexical-binding: t; -*-

;;; Theme

;; Required plugin package
(use-package doom-themes
  :demand t
  :ensure nil
  :defines (doom-themes-enable-blod doom-themes-enable-italic)
  :config
  (setq doom-themes-enable-bold t      ; enable blod
        doom-themes-enable-italic t)   ; enable italic
  (doom-themes-org-config) ; org-mode
  )

;; variable
(defvar lye-light-theme nil
  "Bright color theme used.")
(defvar lye-dark-theme nil
  "Dark theme used.")

;; functions
;;@see https://emacs-china.org/t/emacs-theme/7781
(defun exchange-bright-and-dark-theme ()
  "Use a specific topic for a specific time period."
  (let ((hour (string-to-number (substring (current-time-string) 11 13)))
        (current-theme (car custom-enabled-themes))
        (now-theme nil))
    (if (member hour (number-sequence 8 19))
        (setq now-theme lye-light-theme)
      (setq now-theme lye-dark-theme))
    (when (and now-theme (not (eq current-theme now-theme)))
      (let ((progress-reporter
             (make-progress-reporter
              (format "Loading theme %s..." now-theme))))
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme now-theme t)
        (progress-reporter-done progress-reporter)))))

(when (display-graphic-p)
  ;;Set bright and dark theme
  (setq lye-light-theme 'doom-solarized-light)
  (setq lye-dark-theme 'doom-one)

  (run-with-timer 0 3600 'exchange-bright-and-dark-theme))

(provide 'init-theme)
;;; init-theme.el ends here
