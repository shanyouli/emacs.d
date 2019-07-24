;;; core-modeline.el --- Initialize modeline -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v.01
;; Package-Requires: (awesome-tray)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: modeline


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

;; Mode-line

;;; Code:

;; (defconst default-modelinhfqe-format mode-line-format
;;   "Emacs default mode-line format.")
;; (defconst default-modeline-identification mode-line-buffer-identification
;;   "Emacs default mode-line identification")
;; (defconst default-modeline-mule-info mode-line-mule-info
;;   "Emacs default mode-line mule-info")


(unless after-init-time
  (setq mode-line-format nil))

(if (display-graphic-p)
    (progn
      (require 'awesome-tray)
      (setq awesome-tray-active-modules  '("pyim" "location"  "parent-dir"  "mode-name" "awesome-tab" "date"))
      (advice-add #'awesome-tray-enable :after
                  (lambda ()
                    (setq-default mode-line-buffer-identification nil)
                    (setq-default mode-line-mule-info nil)))
      (run-with-idle-timer 0.1 nil (lambda ()
                               (awesome-tray-mode 1))))

  ;; use setq-default to set it for /all/ modes
  ;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
  (setq-default mode-line-format
        (list
         ;; the buffer name; the file name as a tool tip
         '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                             'help-echo (buffer-file-name)))

         ;; line and column
         "(" ;; '%02' to set to 2 chars at least; prevents flickering
         (propertize "%02l" 'face 'font-lock-type-face) ","
         (propertize "%02c" 'face 'font-lock-type-face)
         ") "

         ;; relative position, size of file
         "["
         (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
         "/"
         (propertize "%I" 'face 'font-lock-constant-face) ;; size
         "] "

         ;; the current major mode for the buffer.
         "["

         '(:eval (propertize "%m" 'face 'font-lock-string-face
                             'help-echo buffer-file-coding-system))
         "] "


         "[" ;; insert vs overwrite mode, input-method in a tooltip
         '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                             'face 'font-lock-preprocessor-face
                             'help-echo (concat "Buffer is in "
                                                (if overwrite-mode "overwrite" "insert") " mode")))

         ;; was this buffer modified since the last save?
         '(:eval (when (buffer-modified-p)
                   (concat ","  (propertize "Mod"
                                            'face 'font-lock-warning-face
                                            'help-echo "Buffer has been modified"))))

         ;; is this buffer read-only?
         '(:eval (when buffer-read-only
                   (concat ","  (propertize "RO"
                                            'face 'font-lock-type-face
                                            'help-echo "Buffer is read-only"))))
         "] "

         ;; add the time, with the date and the emacs uptime in the tooltip
         '(:eval (propertize (format-time-string "%H:%M")
                             'help-echo
                             (concat (format-time-string "%c; ")
                                     (emacs-uptime "Uptime:%hh"))))
         " --"
         ;; i don't want to see minor-modes; but if you want, uncomment this:
         ;; minor-mode-alist  ;; list of minor modes
         "%-" ;; fill with '-'
         )))

(provide 'core-modeline)

;;; core-modeline.el ends here
