;;; md-shackle.el ---Window Configurations.        -*- lexical-binding: t; -*-

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

;; Enforce rules for popups
;; @see https://github.com/seagle0128/.emacs.d/blob/f8a53fbc60414c98142e0fb9d33c8e6353db9347/lisp/md-window.el#L127
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
          ("*color-rg*" :size 0.382 :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.382 :align 'below)
          ;; ("*One-Key*"  :size 0.382 :align 'below :autoclose t)
          ("*SDCV*" :size 0.382 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.382 :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.382 :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.382 :align 'below :autoclose t)
          ("*Flycheck errors*" :size 0.382 :align 'below :autoclose t)
          ("*AWESHELL-DEDICATED*" :size 0.382 :align 'below :autoclose t)
          ("*MULTI-TERM-DEDICATED*" :size 0.382 :align 'below :autoclose t)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.382 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.382 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.382 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below))))

(provide 'md-shackle)
;;; md-shackle.el ends here
