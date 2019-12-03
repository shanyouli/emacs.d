;;; iex-fcitx.el --- Initialize fcitx.el -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (fcitx)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: input-method-tools
;; Last-Updated: 2019-12-01 16:49:58


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

;; Initialize fcitx

;;; Change log:
;;
;; 12/01/19

;;; Code:

(require 'fcitx)

;; (setq fcitx-use-dbus nil)

(fcitx-prefix-keys-add "C-h")
(fcitx-aggressive-setup)
(with-eval-after-load 'pyim
  (defvar fcitx--use-in-pyim nil)
  (add-hook! 'pyim-active-hook
      (when (fcitx--active-p)
        (setq fcitx--use-in-pyim t)
        (fcitx--deactivate)))
  (add-hook! 'input-method-deactivate-hook
      (when fcitx--use-in-pyim
        (fcitx--activate)
        (setq fcitx--use-in-pyim nil))))

(provide 'iex-fcitx)

;;; iex-fcitx.el ends here
