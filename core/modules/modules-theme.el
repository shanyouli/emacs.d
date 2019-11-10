;;; modules-theme.el --- Theme Configuration -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.2
;; Package-Requires: (solar)
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

;;; Change log:
;;  2019/11/10:
;;        * Add latitude and longitude acquisition method using a switching time of light
;;          and dark theme, use `mdt/get-light-and-dark-time' function
;;        * add depends `solar'
;;        * Adjusting `mdt/load-dark-after-some-time+' function
;;        * Adjusting `mdt/load-theme' function,
;;        * Is `mdt/load-theme-from-all' can be run interactively
;;        * fix Compare wrong time, and when it exceeds sunset, the theme of the error can not be replaced.
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

  (if (and (not (member theme custom-enabled-themes))
           (member theme (custom-available-themes)))
      (let ((progress-reporter (make-progress-reporter
                                (format "Loading theme %s..." theme))))
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme theme t)
        (progress-reporter-done progress-reporter)))
  (message "`%s' theme was not there, or has been loaded." theme))

(defun mdt/load-theme-from-all (theme)
  "Setting the choice of theme for all theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (custom-available-themes)))))
  (let ((mdt-theme-list nil))
    (mdt/load-theme theme)))



(defun mdt/run-after-load-theme-hook+ (&rest _)
  "Run `mdt-after-load-theme-hook'"
  (run-hooks 'mdt-after-load-theme-hook))

(advice-add #'load-theme :after #'mdt/run-after-load-theme-hook+)



(defun mdt/time-number-to-string (time-number)
  "Conver `time-number' hours to HH:MM. eg: Convert 16.8 hours to 16:48"
    (let* ((time-integer (truncate time-number))
         (time-decimal (truncate (* 60 (- time-number time-integer)))))
      (concat (and (< time-integer 10) "0")
              (number-to-string time-integer)
            ":"
            (and (< time-decimal 10) "0")
            (number-to-string time-decimal))))

(defun mdt/time-string-to-list (time-list)
  "Convert HH:MM:SS to (HH MM SS)."
  (mapcar #'string-to-number (split-string time-list ":")))

(defun mdt/get-light-and-dark-time ()
  "If the type is a string `mdt-theme-switch-time',
then return itself to return the sunrise and sunset time and vice versa."
  (if (stringp (car mdt-theme-switch-time))
      mdt-theme-switch-time
    (require 'solar)
    (setq calendar-latitude (car mdt-theme-switch-time)
          calendar-longitude (cdr mdt-theme-switch-time))
    (let* ((time-sunrise-sunset (solar-sunrise-sunset (calendar-current-date)))
       (time-sunrise (caar time-sunrise-sunset))
       (time-sunset (caadr time-sunrise-sunset)))
       (cons (mdt/time-number-to-string time-sunrise)
             (mdt/time-number-to-string time-sunset)))))

(defun mdt/time-minus (time-list1 time-list2)
  "Two subtraction time."
  (let ((time-1 (mdt/time-string-to-list time-list1))
        (time-2 (mdt/time-string-to-list time-list2)))
    (- (* 60 (+ (* (- (car time-1) (car time-2)) 60)
                (- (cadr time-1) (cadr time-2))))
       (or (caddr time-2) 0))))

(defun mdt/load-dark-after-some-time+ (&optional time-list)
  "After how many seconds to switch the theme of dark."
  (let* ((time-list (or time-list (mdt/get-light-and-dark-time)))
         (ltime (car time-list))
         (dtime  (cdr time-list))
         (ctime (substring (current-time-string) 11 19)))
    (if (string> ltime ctime)
      (- (mdt/time-minus ltime ctime))
    (if (string> dtime ctime)
        (mdt/time-minus dtime ctime)
      (- (- (mdt/time-minus ltime ctime)) (* 24 60 60))))))



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
        (mdt/load-theme-from-all next-theme)
        (run-at-time (abs next-time) nil #'mdt/switch-light-or-dark-theme+))
    (cancel-function-timers #'mdt/switch-light-or-dark-theme+)))

(provide 'modules-theme)

;;; modules-theme.el ends here
