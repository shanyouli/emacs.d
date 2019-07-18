;;; init-multi-term.el --- Initialize ESHELL -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (multi-term)
;; Homepage: homepage
;; Keywords: shell


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

;;; Code:
(require 'multi-term)

(defun multi-term-dedicated-toggle+ ()
  (interactive)
  (multi-term-dedicated-toggle)
  (if (member "*MULTI-TERM-DEDICATED*"
              (mapcar (function buffer-name) (buffer-list)))
      (other-window 1)))

(provide 'init-multi-term)

;;; init-multi-term.el ends here
