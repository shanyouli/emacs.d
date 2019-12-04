;;; iex-ydcv.el --- Initialize ydcv -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: translated
;; Last-Updated: 2019-12-04 19:51:25


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

;;; Change log:
;;
;; 12/04/19

;;; Code:

(require 'youdao-dictionary)
;; Cache documents
(setq url-automatic-caching t)

;; Set file path for saving search history
(setq youdao-dictionary-search-history-file
      (concat lye-emacs-cache-dir "youdaohs"))

;; Enable Chinese word segmentation support (支持中文分词)
(setq youdao-dictionary-use-chinese-word-segmentation t)

(defun youdao-dictionary-search-at-point++ ()
  (interactive)
  (cond
   ((and (display-graphic-p) (> emacs-major-version 25))
    (call-interactively #'youdao-dictionary-search-at-point-posframe))
   ((display-graphic-p)
    (call-interactively #'youdao-dictionary-search-at-point-tooltip))
   (t
    (call-interactively #'youdao-dictionary-search-at-point))))

(provide 'iex-ydcv)

;;; iex-ydcv.el ends here
