;;; init-sh.el ---Sh scripts Initalize Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords: language

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

;; sh-mode Configurations

;;; Code:

;; @see: https://stackoverflow.com/questions/20558402/open-zsh-scripts-in-sh-mode-in-emacs;; (add-hook 'sh-mode-hook
;; (lambda ()
;;   (if (string-match "\\.zsh$" buffer-file-name)
;;       (sh-set-shell "zsh")
;;     (sh-set-shell "bash"))))

(provide 'init-sh)
;;; init-sh.el ends here
