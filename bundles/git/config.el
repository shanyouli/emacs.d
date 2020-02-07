;;; bundles/git/config.el.el -*- lexical-binding: t -*-

;; Magit configuration.
(setq magit-commit-ask-to-stage nil)   ; don't ask stage question
(setq magit-display-buffer-noselect t) ; don't select magit buffer default

;; transient file
(setq transient-history-file
      (concat lye-emacs-cache-dir "transient/history.el"))
(setq transient-values-file
      (concat lye-emacs-cache-dir "transient/values.el"))
(setq transient-levels-file
      (concat lye-emacs-cache-dir "transient/levels.el"))

;; Forge configuration
(setq forge-database-file
      (expand-file-name "forge-database.sqlite" lye-emacs-cache-dir))

;; Make path column have enough space to display.
(setq magit-submodule-list-columns
      '(("Path"    80 magit-modulelist-column-path  nil)
        ("Version" 30 magit-repolist-column-version nil)
        ("Branch"  20 magit-repolist-column-branch  nil)
        ("B<V" 3 magit-repolist-column-unpulled-from-upstream   ((:right-align t)))
        ("B>U" 3 magit-repolist-column-unpushed-to-upstream     ((:right-align t)))
        ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
        ("B>P" 3 magit-repolist-column-unpushed-to-pushremote   ((:right-align t)))
        ("B"   3 magit-repolist-column-branches                 ((:right-align t)))
        ("S"   3 magit-repolist-column-stashes                  ((:right-align t)))))


(with-eval-after-load 'magit
  (when IS-WINDOWS
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (when (fboundp 'transient-append-suffix)
    ;; Add switch: --tags
    (transient-append-suffix 'magit-fetch
      "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))

  ;; Access Git forges from Magit
  (require 'forges nil t)

  ;; Show TODOs in magit
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-mode +1)

  ;; use diff-hl
  (with-eval-after-load 'diff-hl
    (add-hook! 'magit-post-refresh-hook (diff-hl-magit-post-refresh))))
