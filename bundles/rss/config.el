;;; bundles/rss/config.el -*- lexical-binding: t -*-

;; Newsticker-show-news
(setq newsticker-url-list
      '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
        ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
        ("Oremacs" "https://oremacs.com/atom.xml")
        ("EmacsCast" "https://pinecast.com/feed/emacscast")
        ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss"))
      newsticker-dir  (lib-f-join lye-emacs-cache-dir "newsticker"))

;; elfeed
;; (unless (version< (org-version) "9.0")
(setq url-queue-timeout 30
      elfeed-db-directory (lib-f-join lye-emacs-cache-dir "elfeed")
      elfeed-show-entry-switch #'pop-to-buffer
      elfeed-show-entry-delete #'delete-window
      elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
                     ("http://www.masteringemacs.org/feed/" mastering)
                     ("https://oremacs.com/atom.xml" oremacs)
                     ("https://pinecast.com/feed/emacscast" emacscast)
                     ("https://manateelazycat.github.io/feed.xml" lazycat)
                     ("https://www.reddit.com/r/emacs.rss" reddit)))
(with-eval-after-load 'recentf
  (with-eval-after-load 'elfeed
    (push elfeed-db-directory recentf-exclude)))
