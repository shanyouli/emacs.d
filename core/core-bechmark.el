;;; core-bechmark.el --- Initialize core-bechmark -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (esup )
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: debug


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

;; debug

;;; Code:


;; benchmark-init
(defcustom lye-enable-benchmark-p nil
  "Enable the init benchmark or not."
  :type 'boolean)

;; Configurations
(when lye-enable-benchmark-p
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate))

(provide 'core-bechmark)

;;; core-bechmark.el ends here
