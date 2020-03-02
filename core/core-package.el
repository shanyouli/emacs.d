;;; core/core-package.el.el -*- lexical-binding: t -*-
(add-hook! 'after-init-hook
  :defer 0.1
  :append t
  ;; Start server
  ;; @see https://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
  ;; @see https://emacs-china.org/t/use-package/12051/4?u=shanyouli
  (when (and (boundp 'server-process) server-process
             (not IS-WINDOWS))
    (ignore-errors (server-start)))

  ;; Save cursor position for everyfile you opened. So,  next time you open
  ;; the file, the cursor will be at the position you last opened it.
  (save-place-mode +1)

  ;; Miantain a history of past actions and a resonable number of lists
  (setq-default history-length 1000)
  (setq enable-recursive-minibuffers t
        history-delete-duplicates t
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode +1)

  ;; Save recentf file and open them
  (setq recentf-max-saved-items 200
        ;;Do not add these files to the recently opened text
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          "bookmarks"
                          "ido.*"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'"
                          "COMMIT_MSG"
                          "\\/sudo:root\\@localhost:*"
                          "\\/sudo:root\\@*"))
  (recentf-mode +1)

  ;; Automatically refresh files that have been changed elsewhere
  (global-auto-revert-mode +1)

  ;; Use undo-tree
  ;;(global-undo-tree-mode +1)

  ;; Save Emacs buffers when they lose focus after 2s
  (with-eval-after-load 'super-save
      (cl-pushnew 'split-window-below super-save-triggers)
      (cl-pushnew 'split-window-right super-save-triggers))
  (super-save-mode +1)
  ;; Displays the key bindings following your currently entered
  ;; incomplete command
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  (which-key-mode +1)

  ;; not use mouse
  (when (display-graphic-p) (global-disable-mouse-mode +1))

  ;; Highlight diff
  (autoload 'global-diff-hl-mode "diff-hl")
  (global-diff-hl-mode +1))
