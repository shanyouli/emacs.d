;;; lib-modeline.el --- Mode-Line Configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:ui,mode-line

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

;; Mode-Line Configurations

;;; Code:

(defun lib-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*\\'" (buffer-name)))
      (if (buffer-modified-p)
          (propertize "+ " 'face 'lib-modeline-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "R " 'face 'lib-modeline-read)
          "  "))
    "  "))


(provide 'lib-modeline)
;;; lib-modeline.el ends here
