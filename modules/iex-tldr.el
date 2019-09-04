;;; iex-tldr.el --- Initialize tldr -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (tldr)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tldr-tools


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

;; Tldr client written with emacsc-lisp
;; Time: 2019.09.03 22:10
;;; Code:

(setq-default tldr-directory-path (expand-file-name "tldr/"
                                                    lye-emacs-cache-dir)
              ;; request storage directory
              request-storage-directory (expand-file-name "request"
                                                          lye-emacs-cache-dir))

(package! 'tldr t)

(provide 'iex-tldr)

;;; iex-tldr.el ends here
