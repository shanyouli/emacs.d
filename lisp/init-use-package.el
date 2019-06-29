;;; init-use-package.el --- Initialize use-package packages -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (use-package bind d)
;; Homepage: homepage
;; Keywords: package-manager


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

;; Setup `use-package'
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))


(eval-when-compile (require 'use-package))
;; Extensions
(use-package diminish :ensure nil)
(use-package bind-key :ensure nil)

(provide 'init-use-package)

;;; init-use-package.el ends here
