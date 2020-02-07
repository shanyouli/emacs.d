;;; bundles/tools/config.el.el -*- lexical-binding: t -*-


;; A Simple and cool pomodoro timer
(with-eval-after-load 'all-the-icons
  (setq alert-severity-colors
        `((urgent   . ,(face-foreground 'all-the-icons-red))
          (high     . ,(face-foreground 'all-the-icons-orange))
          (moderate . ,(face-foreground 'all-the-icons-yellow))
          (normal   . ,(face-foreground 'all-the-icons-green))
          (low      . ,(face-foreground 'all-the-icons-blue))
          (trivial  . ,(face-foreground 'all-the-icons-purple)))))

(with-eval-after-load 'pomidor
  (setq pomidor-seconds (* 25 60)) ; 25 minutes for the work period
  (setq pomidor-break-seconds (* 5 60)) ; 5 minutes break time

  ;;sounds config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav")))

  (if (executable-find "notify-send")
      (setq alert-default-style 'libnotify)))


;; TLDR configurations
(setq tldr-directory-path (lib-f-join lye-emacs-cache-dir "tldr")
      request-storage-directory (lib-f-join lye-emacs-cache-dir "request"))
