;;; iex-youdao-dictionary.el --- Init youdao-dictionary -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (youdao-dictionary &optional posframe)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: tranlations

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

(package! 'youdao-dictionary t)

;; Cache documents
(setq url-automatic-caching t)

;; Set file path for saving search history
(setq youdao-dictionary-search-history-file
      (concat lye-emacs-cache-dir "youdaohs"))

;; Enable Chinese word segmentation support (支持中文分词)
(setq youdao-dictionary-use-chinese-word-segmentation t)

(defun youdao-dictionary-search-at-point-posframe ()
  "Search word at point and display result with posframe."
  (interactive)
  (if (locate-library "posframe")
      (progn
         (require 'posframe)
        (let ((word (youdao-dictionary--region-or-word)))
          (if word
              (progn
                (posframe-show "youdao-dict-buffer"
                               :foreground-color (face-background 'default)
                               :background-color (face-foreground 'default)
                               :string (youdao-dictionary--format-result word)
                               :position (point))
                (unwind-protect
                    (push (read-event) unread-command-events)
                  (posframe-hide "youdao-dict-buffer")))
            (message "Nothing to look up"))))
    (message "Please install `posframe'")))

(defun youdao-dictionary-search-at-point++ ()
  (interactive)
  (cond
   ((and (display-graphic-p) (> emacs-major-version 25))
    (call-interactively #'youdao-dictionary-search-at-point-posframe))
   ((display-graphic-p)
    (call-interactively #'youdao-dictionary-search-at-point-tooltip))
   (t
    (call-interactively #'youdao-dictionary-search-at-point))))

(provide 'iex-youdao-dictionary)

;;; iex-youdao-dictionary.el ends here
