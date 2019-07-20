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

;;(require 'awesome-tray "~/.emacs.d/site-lisp/awesome-tray/awesome-tray.el")
(require 'awesome-tray "~/Repo/awesome-tray/awesome-tray.el")
(defconst default-modelinhfqe-format mode-line-format
  "Emacs default mode-line format.")
(defconst default-modeline-identification mode-line-buffer-identification
  "Emacs default mode-line identification")
(defconst default-modeline-mule-info mode-line-mule-info
  "Emacs default mode-line mule-info")

(unless after-init-time
  (setq mode-line-format nil))
(setq awesome-tray-active-modules  '("pyim" "location"  "parent-dir"  "mode-name" "awesome-tab" "date"))
(run-with-idle-timer 0.1 nil (lambda ()
                             (setq-default mode-line-buffer-identification nil)
                             (setq-default mode-line-mule-info nil)
                             (awesome-tray-mode 1)))

(provide 'core-modeline)

;;; core-modeline.el ends here
