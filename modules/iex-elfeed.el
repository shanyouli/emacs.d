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
(require 'elfeed)
(setq elfeed-db-directory (concat lye-emacs-cache-dir "elfeed"))

(setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast"
          "https://emacs-china.github.io/rss.xml"
          "https://manateelazycat.github.io/feed.xml"))

(pretty-hydra-define toggle-hydra-elfeed
  (:title (pretty-hydra-title "Elfeed" 'facion "rss-square")
   :color amaranth :quit-key "q")
  ("Search"
   (("g" elfeed-search-update--force "Refresh")
    ("G" elfeed-search-fetch "update")
    ("y" elfeed-search-yank "Copy URL")
    ("+" elfeed-search-tag-all "Tag all")
    ("-" elfeed-search-untag-all "untag all"))
   "Filter"
   (("s" elfeed-search-live-filter "Live filter")
    ("S" elfeed-search-set-filter "Set filter")
    ("*" (elfeed-search-filter "@6-months-ago +star") "Starred")
    ("A" (elfeed-search-set-filter "@6-moths-ago") "All")
    ("T" (elfeed-search-set-filter "@1-day-ago") "today"))
   "Article"
   (("b" elfeed-search-browse-url "Browse")
    ("n" next-line "Next")
    ("p" previous-line "Previous")
    ("u" elfeed-search-untag-all-unread "mark unread")
    ("r" elfeed-search-untag-all-read "mark read")
    ("RET" elfeed-search-show-entry "Show"))
))

(lazy-load-local-keys '(("?" . toggle-hydra-elfeed/body)) elfeed-search-mode-map "iex-elfeed")

(provide 'iex-elfeed)

;;; iex-elfeed.el ends here
