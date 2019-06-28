;;; init-windows.el ---Window Configurations.        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:

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

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :bind (("C-x 4 u" . winner-undo)
         ("C-x 4 r" . winner-redo)))

;; Quickly switch windows
(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :hook (after-init . ace-window-display-mode)
  :custom-face
  (aw-leading-char-face
   ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t :inherit mode-line-emphasis :bold t))))


;; Enforce rules for popups
;; @see https://github.com/seagle0128/.emacs.d/blob/f8a53fbc60414c98142e0fb9d33c8e6353db9347/lisp/init-window.el#L127
(use-package shackle
  :commands shackle-display-mode
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '(("*Help*" :select t :size 0.382 :align 'below :autoclose t)
          ("*completions*" :select t :size 0.382 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 0.382 :align 'below)
          ("*Warnings*" :select t :size 0.382 :align 'below)
          ("*Messages*" :size 0.382 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.382 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.382 :align 'below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.382 :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.382 :align 'below :autoclose t)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.382 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.382 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.382 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below))))


(provide 'init-window)
;;; init-windows.el ends here
