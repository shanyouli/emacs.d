;;; iex-window.el --- Window Configurations -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1.0
;; Package-Requires: (ace-window winner)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: windows


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

;; Window cpnfigurations

;;; Code:

;; Quickly switc windows
;; (custom-set-faces
;;  '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
;;  '(aw-mode-line-face ((t :inherit mode-line-emphasis :bold t))))

(require 'winner)
(package! 'ace-window t)

(set-face-attribute 'aw-leading-char-face nil
                    :inherit font-lock-keyword-face
                    :bold t
                    :height 3.0)

;; (set-face-attribute 'aw-mode-line-face t
;;                     :inherit mode-line-emphasis
;;   :bold t)

;; run
(winner-mode t)
(ace-window-display-mode t)

(provide 'iex-window)

;;; iex-window.el ends here
