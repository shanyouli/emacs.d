;;; init-aweshell.el --- Initialize ESHELL -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (aweshell)
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

(use-package aweshell
  :ensure nil
  :commands (aweshell-toggle aweshell-new aweshell-next aweshell-prev
                             aweshell-sudo-toggle aweshell-switch-buffer))

(provide 'init-aweshell)

;;; init-aweshell.el ends here
