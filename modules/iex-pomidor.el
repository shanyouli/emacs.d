;;; iex-pomidor.el --- Tomato clock plan -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (pomidor)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: Time Plan


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

;; Tomato clock

;;; Code:

(require-package 'pomidor)
(require 'pomidor)

(setq pomidor-seconds (* 25 60)) ; 25 minutes for the work period
(setq pomidor-break-seconds (* 5 60)) ; 5 minutes break time

;;sounds config
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav")))

;; for a full list of available faces see `customize' or search for `defface' in the source code
;; (set-face-attribute 'pomidor-break-face nil :foreground "#00ff00")
;; (set-face-attribute 'pomidor-overwork-face nil :foreground "#00abff")
;; (set-face-attribute 'pomidor-skip-face nil :foreground "#abbac3")
;; (set-face-attribute 'pomidor-work-face nil :foreground "#ff0000")

;; Sound player
(when (executable-find "mpv")
  (setq pomidor-play-sound-file
        (lambda (file)
          (start-process "my-pomidor-play-sound" nil  "mpv" file))))

;; notifications
(when (locate-library "alert")
  (if (executable-find "notify-send")
      (setq alert-default-style 'libnotify))

  ;;(setq pomidor-alert (lambda () (alert "OMG!11")))
  )

(provide 'iex-pomidor)

;;; iex-pomidor.el ends here
