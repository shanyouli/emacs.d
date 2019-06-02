;;; init-theme.el --- Initialize theme configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:theme

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

;; Theme
;; Understand the topics currently in use
(defun lye/current-theme ()
  "what is the Current theme?"
  (interactive)
  (message "The Current theme is %s"
           (substring (format "%s" custom-enabled-themes) 1 -1)))

(if (display-graphic-p)
    (use-package doom-themes
      :init (load-theme 'doom-one t)))

;; mode-line
(if (display-graphic-p)
    (use-package doom-modeline
      :hook  (after-init . doom-modeline-mode)
      :init
      ;; Only display the file name
      (setq doom-modeline-buffer-file-name-style 'truncate-upto-root))
  (require 'lazycat-theme))

(provide 'init-theme)
;;; init-theme.el ends here
