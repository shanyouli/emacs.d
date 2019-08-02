;;; iex-speed-type.el --- Typing test -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (speed-type)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: speed test


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

;; Speed Test

;;; Code:

(require-package 'speed-type)
(setq speed-type-gb-dir (concat lye-emacs-cache-dir "speed-type"))
(require 'speed-type)

(provide 'iex-speed-type)

;;; iex-speed-type.el ends here
