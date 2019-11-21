;;; lex-ido.el --- Initialize smex -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.2
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: M-x


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

;; Ido mode--Does not contain other uses of packages that need to be installed

;;; Code:
(require 'ido)
(require 'ido-completing-read+)
(require 'smex)
(require 'flx-ido)
(require 'ido-sort-mtime)

(setq ido-enable-flex-matching  t
      ido-everywhere            nil
      ido-use-filename-at-point 'guess
      ido-create-new-buffer     'always
      ido-max-prospects         10
      ido-save-directory-list-file (expand-file-name "ido.hist" lye-emacs-cache-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)

;; smex, remember recently and most frequently ised commands
(let ((amx-file (expand-file-name "amx-items" lye-emacs-cache-dir)))
  (if (file-exists-p amx-file)
      (setq smex-save-file amx-file)
    (setq smex-save-file (expand-file-name "smex-items" lye-emacs-cache-dir))))
(setq smex-history-length 10)
(smex-initialize)

;;; ido-sort-mtime
(ido-sort-mtime-mode +1)

(provide 'lex-ido)

;;; lex-ido.el ends here
