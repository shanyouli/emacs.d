;;; lib-themes.el --- One theme switch tool          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: theme,tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; One theme switch tool

;;; Code:

(defgroup lib-themes nil "Theme configuration." :group 'lib-themes)

(defcustom lib-theme-list nil "Custom theme list." :type 'list :group 'lib-themes)

(defcustom lib-theme-switch nil
  "The default theme dark theme Choose. `(light-theme . dark-theme)"
  :type 'list
  :group 'lib-themes)

(defcustom lib-theme-switch-time nil
  "Light and dark theme exchange time.
eg: `(08:30 . 19:30)' Indication LIGHT-THEME between 08:30 to 19:30,
the remaining time using `dark-theme'"
  :type 'list
  :group 'ib-themes)



(defun lib-theme--load (theme)
  "Loading theme."
  (if (and (not (member theme custom-enabled-themes))
           (member theme (custom-available-themes)))
      (let ((progress-reporter (make-progress-reporter
                                (format "Loading theme %s..." theme))))
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme theme t)
        (progress-reporter-done progress-reporter))
    (message "`%s' theme was not there, or has been loaded." theme)))

(defun lib-theme--time-number-to-string (timer-number)
  "Conver TIME-NUMBER hours to HH:MM. eg: Covert 16.8 hours to 16:48."
  (let* ((time-integer (truncate timer-number))
         (time-decimal (truncate (* 60 (- timer-number time-integer)))))
    (concat (and (< time-integer 10) "0")
            (number-to-string time-integer)
            ":"
            (and (< time-decimal 10) "0")
            (number-to-string time-decimal))))

(defvar lib-theme--sunrise-and-sunset-time '(8.5 . 18.5))

(defun lib-theme--get-sunrise-and-sunset-time (lat long)
  "Sunrise and sunset time using latitude and longitude calculations."
  (require 'solar)
  ;; (setq calendar-latitude lat)
  ;; (setq calendar-longitude long)
  (let ((calendar-latitude lat)
        (calendar-longitude long)
         time-sunrise-sunset
         sunrise-time sunset-time)
    (setq time-sunrise-sunset (solar-sunrise-sunset (calendar-current-date)))
    (setq sunrise-time (if (car time-sunrise-sunset)
                           (caar time-sunrise-sunset)
                         (car lib-theme--sunrise-and-sunset-time)))
    (setq sunset-time (if (cadr time-sunrise-sunset)
                          (caadr time-sunrise-sunset)
                        (cdr lib-theme--sunrise-and-sunset-time)))
    (cons (lib-theme--time-number-to-string sunrise-time)
          (lib-theme--time-number-to-string sunset-time))))

(defun lib-theme--get-swith-times ()
  "When the element of `lib-theme-switch-time' is a string.
Return `lib-theme-switch-time'.
When the element of `lib-theme-switch-time' is a numberp.
Using the function returns the result of the process
`lib-theme--get-sunrise-and-sunset-time'."
  (pcase (car lib-theme-switch-time)
    ((pred stringp) lib-theme-switch-time)
    ((pred numberp) (lib-theme--get-sunrise-and-sunset-time
                     (car lib-theme-switch-time)
                     (cdr lib-theme-switch-time)))
    (_ (error "Cannot make into the symbol: %s." lib-theme-switch-time))))

(defun lib-theme--string-time-to-list (time-list)
  "Convert HH:MM:SS to (HH MM SS)."
  (mapcar #'string-to-number (split-string time-list ":")))

(defun lib-theme--time-to-seconds (time-list1 time-list2)
  "Two subtraction time."
  (let ((time-1 (lib-theme--string-time-to-list time-list1))
        (time-2 (lib-theme--string-time-to-list time-list2)))
    (- (* 60 (+ (* (- (car time-1) (car time-2)) 60)
                (- (cadr time-1) (cadr time-2))))
       (or (caddr time-2) 0))))

(defun lib-theme--load-need-time (start-time end-time)
  "When result > 0, the elapsed restult seconds to reach the END-TIME.
When result < 0, elapsed result seconds to reach the START-TIME."
  (let ((ctime (substring (current-time-string) 11 19)))
    (if (string> start-time ctime)
        (- (lib-theme--time-to-seconds start-time ctime))
      (if (string> end-time ctime)
          (lib-theme--time-to-seconds end-time ctime)
        (- (+ (lib-theme--time-to-seconds start-time ctime)
              (* 24 60 60)))))))

(defun lib-theme-switch-theme (&optional forcep)
  "Change the light and dark themes over time.
FORCEP is non-nil, Enforcement functions."
  (if (or forcep
          (not custom-enabled-themes)
          (memq (car custom-enabled-themes) lib-theme-switch))
      (let* ((next-time (lib-theme--get-swith-times))
             (next-time (lib-theme--load-need-time (car next-time) (cdr next-time)))
             next-theme)
        (setq next-theme (if (> next-time 0)
                             (cadr lib-theme-switch)
                           (cadr lib-theme-switch)))
        (cancel-function-timers #'lib-theme-switch-theme)
        (lib-theme--load next-theme)
        (run-at-time (abs next-time) nil  #'lib-theme-switch-theme))
    (cancel-function-timers #'lib-theme-switch-theme)))

;;;###autoload
(defun lib-theme/list-load (theme)
  "Loading theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (or lib-theme-list (custom-available-themes))))))
  (lib-theme--load theme))

;;;###autoload
(defun lib-theme--random-load (themes)
  "Pickup random color theme from themes."
  (let ((theme (nth (random (length themes)) themes)))
    (lib-theme--load theme)))

;;;###autoload
(defun lib-theme/random-load-color ()
  "Random color themes."
  (interactive)
  (lib-theme--random-load (or lib-theme-switch (custom-available-themes))))

(provide 'lib-themes)
;;; lib-themes.el ends here
