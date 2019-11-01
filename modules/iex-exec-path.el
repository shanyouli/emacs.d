;;; iex-exec-path.el --- Initialize exec-path-from-shell -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (exec-path-from-shell cache-path-from-shell)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:exec path


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

;;; Code:
(package! 'exec-path-from-shell t t)
(package! '(cache-path-from-shell :repo "manateelazycat/cache-path-from-shell"
                                  :host github
                                  :type git) t)


(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (setq exec-path-from-shell-arguments '("-i"))
  (exec-path-from-shell-initialize))

(provide 'iex-exec-path)

;;; iex-exec-path.el ends here
