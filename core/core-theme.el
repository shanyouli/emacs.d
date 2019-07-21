;;; core-theme.el --- Initialize THEME -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v01
;; Package-Requires: (Doom-theme)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:theme


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; THEME

;;; Code:

(require 'cl)

;; enable blod and italic
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)


;; Add a hook to load-theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

;; @see https://github.com/guidoschmidt/circadian.el
;; @see https://emacs-china.org/t/emacs-theme/7781
(defun lye/time-to-number (time)
  "Conversion time is a list of numbers."
  (let ((time-to-list (split-string time ":")))
    (cl-map 'list #'string-to-number time-to-list)))

(defun lye/hour-to-minute (time)
  "Convert time to minute representation display.
EG: 11:12 == 11x60+12."
  (let ((time-list (lye/time-to-number time)))
    (+ (* 60 (car time-list)) (car (cdr time-list)))))

(defun lye/current-time ()
  "Input current-time."
  (substring (current-time-string) 11 16))

(defun lye/switch-theme (new-theme)
  (if (and new-theme (member new-theme (custom-available-themes)))
      (let ((progress-reporter (make-progress-reporter
                                (format "Loading theme %s..." new-theme))))
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme new-theme t)
        (progress-reporter-done progress-reporter))))

(defun lye/switch-light-or-dark-theme ()
  "Exchange topics on time."
  (if (or (not custom-enabled-themes)
          (member (car custom-enabled-themes) `(,lye-dark-theme ,lye-light-theme)))
      (let* ((current-time-minute (lye/hour-to-minute (lye/current-time)))
             (light-time-minute (lye/hour-to-minute lye-light-time))
             (dark-time-minute (lye/hour-to-minute lye-dark-time))
             (next-time nil)
             (now-theme nil))
        (cond
         ((and (> current-time-minute light-time-minute)
               (< current-time-minute dark-time-minute))
          (setq next-time (* 60 (- dark-time-minute current-time-minute))
                now-theme lye-light-theme))
         ((and (> current-time-minute light-time-minute)
               (= current-time-minute dark-time-minute))
          (setq next-time (* 60 (- (* 24 60) dark-time-minute))
                now-theme lye-dark-theme))
         ((and (> current-time-minute light-time-minute)
               (> current-time-minute dark-time-minute))
          (setq next-time (* 60 (+ light-time-minute (- (* 24 60) current-time-minute)))
                now-theme lye-dark-theme))
         ((= current-time-minute light-time-minute)
          (setq next-time (- dark-time-minute light-time-minute)
                now-theme lye-light-theme))
         ((< current-time-minute light-time-minute)
          (setq next-time (- light-time-minute current-time-minute)
                now-theme lye-dark-theme)))
        (cancel-function-timers #'lye/switch-theme-light-or-dark-theme)
        (lye/switch-theme now-theme)
        (run-at-time next-time nil #'lye/switch-light-or-dark-theme))

    (cancel-function-timers #'lye/switch-light-or-dark-theme)))


(when (display-graphic-p)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'doom-themes)
              (setq lye-light-theme 'doom-one-light
                    lye-dark-theme 'doom-one
                    lye-light-time  "08:30"
                    lye-dark-time "19:30")
              (lye/switch-light-or-dark-theme)
              ))

  ;;Fixed conflict with awesome-tray when switching emacs-doom-theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (if (and (boundp 'awesome-tray-active-p) awesome-tray-active-p)
                  (awesome-tray-enable)))))

(provide 'core-theme)

;;; core-theme.el ends here
