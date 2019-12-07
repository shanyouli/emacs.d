;;; iex-doom-modeline.el --- Initialize Doom-Modeline -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (doom-modeline)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:
;; Last-Updated: 2019-12-07 13:58:11


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

;; commentary

;;; Change log:
;;
;; 12/07/19

;;; Code:
(require 'doom-modeline)
(unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format '()))

  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil
        doom-modeline-height 10
        doom-modeline-bar-width 2
        doom-modeline-buffer-file-name-style 'truncate-upto-root)

(doom-modeline-mode +1)

(provide 'iex-doom-modeline)

;;; iex-doom-modeline.el ends here
