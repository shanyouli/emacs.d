;;; bundles/git/key.el.el -*- lexical-binding: t -*-

(defonekey magit nil
  "Magit keybindings"
  ("s" magit-status+ "Magit status")
  ("c" magit-checkout "Magit Checkout")
  ("C" magit-commit "Magit commit")
  ("u" magit-push-current-to-pushremote "Magit push to Pushremote")
  ("i" magit-pull-from-upstream  "Magit Pull")
  ("r" magit-rebase "Magit rebase")
  ("e" magit-merge "Megit Merge")
  ("l" magit-log-all "Megit Log")
  ("L" magit-blame+ "Magit blame")
  ("b" magit-branch "Magit branch")
  ("B" magit-process-buffer "Magit branch")
  ("m" magit-submodule-add+ "Magit submodule add")
  ("d" magit-submodule-remove+ "Magit submodule remove")
  ("D" magit-discard "Magit discarded")
  ("." magit-init "Magit init")
  ("," magit-remote-add "Magit add remote"))

;; @https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/md-git.el#L154
  (defun magit-submodule-add+ (url)
    "It's more convenient to add third-party packages that don't use package
management for Emacs."
    (interactive "sURL: ")
    (let ((parent-dir
           (cadr
            (split-string (expand-file-name
                           (file-name-as-directory lye-site-lisp-dir))
                          (expand-file-name (cdr (project-current)))))))
      (magit-submodule-add
       url
       (concat parent-dir (file-name-base url))
       (file-name-base url))))

(defun magit-submodule-remove+ ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))

(defun magit-status+ ()
  (interactive)
  (magit-status)
  (other-window 1))

(defun magit-blame+ ()
  (interactive)
  (setq magit-blame--style
        '(margin
          (margin-format " %s%f" " %C %a" " %H")
          (margin-width . 42)
          (margin-face . magit-blame-margin)
          (margin-body-face magit-blame-dimmed)))
  (magit-blame))

(defun magit-delete-remote-branch ()
  (interactive)
  (when (y-or-n-p (format "Delete remote branch (%s): " (magit-get-current-branch)))
    (magit-run-git-async "push" "origin" (format ":%s" (magit-get-current-branch)))))
