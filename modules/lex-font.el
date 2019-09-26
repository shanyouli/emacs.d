;;; lex-font.el --- Initialize font -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (setup-font)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: font


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

;; font setup

;;; Code:

;;;###autoload
(defun init-font-config (&optional chinese english)
  "font initialization"
  (when (display-graphic-p)
    (setq setup-english-font  "Fantasque Sans Mono"
          setup-cjk-font  (cond
                           (system/linux
                            "WenQuanYi Micro Hei")
                           (system/mac
                            "Hiragio Sans GB")
                           (system/windows
                            "Microsoft Yahei"))
          setup-font-default-size 14)
    (setup-font-initialize)))

(provide 'lex-font)

;;; lex-font.el ends here
