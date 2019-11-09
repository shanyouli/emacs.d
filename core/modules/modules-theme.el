;;; modules-theme.el --- Theme Configuration -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: theme


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

;; Theme Manager
;;; @see https://github.com/guidoschmidt/circadian.el/

;;; Code:

(defgroup modules-theme nil
  "Theme configuration module."
  :group 'modules-theme)

(defcustom mdt-theme-list nil
  "Custom theme list."
  :type 'list
  :group 'modules-theme)

(defcustom mdt-after-load-theme-hook '()
  "A hook run after a color theme is loaded using `load-theme'"
  :type 'hook
  :group 'modules-theme)

(defcustom mdt-theme-light-and-dark '()
  "The default theme dark theme Choose. `(light-theme . dark-theme)'"
  :type 'list
  :group 'modules-theme)

(defcustom mdt-theme-switch-time '()
  "Light and dark theme exchange time.
eg: `(08:30 . 19:30)' Indication `light-theme' between 08:30 to 19:30,
the remaining time using `dark-theme'."
  :type 'list
  :group 'modules-theme)

;;;###autoload
(defun mdt/load-theme (theme)
  "If `mdt-theme-list' exists, then set the theme to A list of theme.
Conversely Theme Settings range `(custom-available-themes)'."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (if mdt-theme-list
                                 mdt-theme-list
                               (custom-available-themes))))))

  (if (and theme (member theme (custom-available-themes)))
      (let ((progress-reporter (make-progress-reporter
                                (format "Loading theme %s..." theme))))
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme theme t)
        (progress-reporter-done progress-reporter))))

(defun mdt/load-theme-from-all! (theme)
  "Setting the choice of theme for all theme."
  (let ((mdt-theme-list nil))
    (mdt/load-theme theme)))

(defun mdt/run-after-load-theme-hook+ (&rest _)
  "Run `mdt-after-load-theme-hook'"
  (run-hooks 'mdt-after-load-theme-hook))

(advice-add #'load-theme :after #'mdt/run-after-load-theme-hook+)

(defun mdt/time-convert-to-seconds! (hour-minute)
  "Converting the `HOUR-MINUTE' is how many seconds.
eg: `12:30' <==> (12*60+30)*60"
  (let ((time-list (mapcar #'string-to-number (split-string hour-minute ":"))))
    (* 60 (+ (* (car time-list) 60) (cadr time-list)))))

(defun mdt/current-time-convert-to-minute+ ()
  "It indicates the current time in minutes."
  (let* ((ctime-str (substring (current-time-string) 11 19))
         (ctime-list (split-string ctime-str ":"))
         (ctime-seconds (mapcar #'string-to-number ctime-list)))
    (+ (* (+ (* 60 (car ctime-seconds)) (cadr ctime-seconds)) 60)
       (caddr ctime-seconds))))

(defun mdt/load-dark-after-some-time+ (&optional time-list)
  "After how many seconds to switch the theme of dark."
  (let* ((time-list (or time-list mdt-theme-switch-time))
         (light-time (mdt/time-convert-to-seconds! (car time-list)))
         (dark-time (mdt/time-convert-to-seconds! (cdr time-list)))
         (ctime (mdt/current-time-convert-to-minute+))
         (ctime-d (- ctime light-time))
         (ctime-l (- dark-time ctime )))
    (if (< ctime-d 0)
        ctime-d
      (if (> ctime-l 0)
          ctime-l
        (- (- ctime (* 24 60 60)) dark-time)))))

;;;###autoload
(defun mdt/switch-light-or-dark-theme+ (&optional theme-list time-list)
  "Change the light and dark themes over time."
  (and theme-list (setq mdt-theme-light-and-dark theme-list))
  (and time-list (setq mdt-theme-switch-time time-list))

  (if (or (not custom-enabled-themes)
          (memq (car custom-enabled-themes) mdt-theme-light-and-dark))
      (let ((next-time (mdt/load-dark-after-some-time+))
             (next-theme))
        (if (> next-time 0)
            (setq next-theme (car mdt-theme-light-and-dark))
          (setq next-theme (cadr mdt-theme-light-and-dark)))
        (cancel-function-timers #'mdt/switch-light-or-dark-theme+)
        (mdt/load-theme-from-all! next-theme)
        (run-at-time (abs next-time) nil #'mdt/switch-light-or-dark-theme+))
    (cancel-function-timers #'mdt/switch-light-or-dark-theme+)))

(provide 'modules-theme)

;;; modules-theme.el ends here
