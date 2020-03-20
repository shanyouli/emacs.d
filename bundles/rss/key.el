;;; bundles/rss/key.el -*- lexical-binding: t -*-

(pretty-hydra-define elfeed-hydra
    (:title (pretty-hydra-title "Elfeed"  'faicon "rss-square")
     :color amaranth :quit-key "q")
  ("Search"
   (("c" elfeed-db-compact "compact db")
    ("g" elfeed-search-update--force "refresh")
    ("G" elfeed-search-fetch "update")
    ("y" elfeed-search-yank "copy URL")
    ("+" elfeed-search-tag-all "tag all")
    ("-" elfeed-search-untag-all "untag all"))
   "Filter"
   (("s" elfeed-search-live-filter "live filter")
    ("S" elfeed-search-set-filter "set filter")
    ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
    ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
    ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
   "Article"
   (("b" elfeed-search-browse-url "browse")
    ("n" next-line "next")
    ("p" previous-line "previous")
    ("u" elfeed-search-tag-all-unread "mark unread")
    ("r" elfeed-search-untag-all-unread "mark read")
    ("RET" elfeed-search-show-entry "show"))))
