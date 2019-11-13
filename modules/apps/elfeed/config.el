;;; modules/apps/elfeed/config.el -*- lexical-binding: t -*-

(require 'elfeed)

(setq elfeed-db-directory (concat lye-emacs-cache-dir "elfeed"))


(setq elfeed-feeds
        '("http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast"
          "https://emacs-china.github.io/rss.xml"
          "https://manateelazycat.github.io/feed.xml"
          "https://planet.emacslife.com/atom.xml"))
