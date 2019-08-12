;;; lex-smex.el --- Initialize smex -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (smex)
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

;; Smex configuration

;;; Code:

(require 'smex)
(let ((amx-file (expand-file-name "amx-items" lye-emacs-cache-dir)))
  (if (file-exists-p amx-file)
      (setq smex-save-file amx-file)
    (setq smex-save-file (expand-file-name "smex-items" lye-emacs-cache-dir))))
(setq smex-history-length 10)

(smex-initialize)

(provide 'lex-smex)

;;; lex-smex.el ends here
