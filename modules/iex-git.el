;;; iex-git.el --- Init for git -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (magit magithub)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: git


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

;; Init for git

;;; Code:

(require-package 'magit)
(require-package 'forge)
(require 'magit)
(require 'forge)

;; PATH
(lye/exec-path-from-shell-init)

;; Magit configuration.
(setq magit-commit-ask-to-stage nil)   ; don't ask stage question
(setq magit-display-buffer-noselect t) ; don't select magit buffer default

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

;; unset C-x g
;; @see https://github.com/magit/magit/issues/3522#issuecomment-407640436
(with-eval-after-load "magit-files"
  (define-key magit-file-mode-map (kbd "C-x g") nil))

(defvar one-key-menu-magit-alist nil
  "The `one-key' menu alist for MAGIT.")

(setq one-key-menu-magit-alist
      '((("s" . "Magit Status")               . magit-status+)
        (("c" . "Magit checkout")             . magit-checkout)
        (("C" . "Magit Commit")               . magit-commit)
        (("u" . "Magit push to Pushremote")   . magit-push-current-to-pushremote)
        (("p" . "Magit Delete remote branch") . magit-delete-remote-branch)
        (("i" . "Magit Pull")                 . magit-pull-from-upstream)
        (("r" . "Magit rebase")               . magit-rebase)
        (("e" . "Magit Merge")                . magit-merge)
        (("l" . "Magit Log")                  . magit-log-all)
        (("L" . "Magit blame")                . magit-blame+)
        (("b" . "Magit branch")               . magit-branch)
        (("B" . "Magit buffer")               . magit-process-buffer)
        (("m" . "Magit submodule add")        . magit-submodule-add+)
        (("d" . "Magit submodule remove")     . magit-submodule-remove+)
        (("M" . "Magit submodule list")       . magit-list-submodules)
        (("D" . "Magit discarded")            . magit-discard)
        (("," . "Magit init")                 . magit-init)
        (("." . "Magit add remote")           . magit-remote-add)
        ))

(defun one-key-menu-magit ()
  "The `one-key' menu for MAGIT."
  (interactive)
  (one-key-menu "MAGIT" one-key-menu-magit-alist t))

;; @https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-git.el#L154
  (defun magit-submodule-add+ (url)
    "It's more convenient to add third-party packages that don't use package
management for Emacs."
    (interactive "sURL: ")
    (let ((parent-dir
           (cadr
            (split-string (expand-file-name
                           (file-name-as-directory lye-emacs-site-lisp-dir))
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

(provide 'iex-git)

;;; iex-git.el ends here
