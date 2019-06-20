;;; init-theme.el --- Initialize theme configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:theme

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

;;

;;; Code:
;; https://github.com/honmaple/dotfiles/blob/master/emacs.d/site-lisp/maple/maple-theme.el

;; Theme
(use-package doom-themes
  :defines (doom-themes-enable-blod doom-themes-enable-italic)
  :config
  (setq doom-themes-enable-bold t      ; enable blod
        doom-themes-enable-italic t)   ; enable italic
  (doom-themes-org-config) ; org-mode
  )
(use-package zenburn-theme)
(use-package material-theme)
(use-package dracula-theme)


(defvar lye-emacs-theme-list '(doom-molokai
                               doom-one
                               zenburn
                               material
                               dracula)
  "Some theme cycle!")

(defvar lye-emacs-theme 'doom-one)

(defun lye/theme-cycle (&optional step)
  "Loop emacs theme!"
  (let ((next-theme lye-emacs-theme-list))
    (if step
        (progn
          (if (memq (car custom-enabled-themes) next-theme)
              (progn
                (setq next-theme (if (< step 0)
                                     (reverse lye-emacs-theme-list)
                                   lye-emacs-theme-list))

                (setq lye-emacs-theme
                      (or (cadr (member lye-emacs-theme next-theme))
                          (car next-theme)))
                (when lye-emacs-theme
                  (let ((progress-reporter
                         (make-progress-reporter
                          (format "Loading theme %s..." lye-emacs-theme))))
                    (mapc 'disable-theme custom-enabled-themes)
                    (load-theme lye-emacs-theme t)
                    (progress-reporter-done progress-reporter))))
            (message "The current theme does not belong to one of lye-emacs-theme-list.
 To loop, add it to lye-emacs-theme-list.")))

      ;; (substring (format "%s" custom-enabled-themes) 1 -1)
      (message "The Current theme is %s" (car custom-enabled-themes))
      )))

;; Understand the topics currently in use
(defun lye/current-theme ()
  "what is the Current theme?"
  (interactive)
  (lye/theme-cycle))

(defun lye/next-theme ()
  "Netx theme!"
  (interactive)
  (lye/theme-cycle 1))

(defun lye/previous-theme ()
  "Previous theme."
  (interactive)
  (lye/theme-cycle -1))

(when (display-graphic-p)

  ;; Mode-line
  (use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :init
    ;; prevent flash of unstyled modeline at startup
    (unless after-init-time
      (setq-default mode-line-format nil))

    (setq doom-modeline-height 25)
    (setq doom-modeline-bar-width 3)

    ;;It seems that you cannot use the all-the-icons font on windows.
    (if (and (boundp 'system/windows) system/windows)
        (setq doom-modeline-icon nil))

    (setq doom-modeline-major-mode-color-icon t
          doom-modeline-minor-modes nil
          doom-modeline-mu4e nil
          doom-modeline-github t
          doom-modeline-github-interval 300))

  (load-theme lye-emacs-theme t))

(unless (display-graphic-p)
  (require 'lazycat-theme))

(provide 'init-theme)
;;; init-theme.el ends here
