;;; iex-elfeed.el --- A RSS feed reader. -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (elfeed)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: elfeed


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

;; A RSS feed reader

;;; Code:

(require-package 'elfeed)

(setq elfeed-db-directory (concat lye-emacs-cache-dir "elfeed"))

(setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast"
          "https://emacs-china.github.io/rss.xml"
          "https://manateelazycat.github.io/feed.xml"))

(provide 'iex-elfeed)

;;; iex-elfeed.el ends here
