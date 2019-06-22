;;; init-magit.el ---Initalize Git Configurations    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package magit
  :commands (magit-submodule-add magit-submodule-remove)

  :preface
  ;; @https://emacs-china.org/t/submodule/6926/6
  (defun magit-submodule-remove+ ()
    "When deleting git-submodule, delete the corresponding residual files
under .git/modules. Note that the default magit-submodule-remove will not delete
the files under .git/modules, as this will delete the user's personal
modifications to the currently deleted module. "
    (interactive)
    (magit-submodule-remove
     (list (magit-read-module-path "Remove module"))
     "--force" nil))

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

  :config
  (setq transient-history-file
        (concat lye-emacs-cache-dir "transient/history.el"))
  (setq transient-values-file
        (concat lye-emacs-cache-dir "transient/values.el"))
  (setq transient-levels-file
        (concat lye-emacs-cache-dir "transient/levels.el"))

  :bind (("C-x g s" . magit-status)
         ("C-x g c" . magit-checkout)
         ("C-x g C" . magit-commit)
         ("C-x g u" . magit-push-current-to-pushremote)
         ("C-x g p" . magit-delete-remote-branch)
         ("C-x g i" . magit-pull-from-upstream)
         ("C-x g e" . magit-merge)
         ("C-x g l" . magit-log-all)
         ("C-x g b" . magit-branch)
         ("C-x g B" . magit-process-buffer)
         ("C-x g m" . magit-submodule-add+)
         ("C-x g d" . magit-submodule-remove+)
         ("C-x g M" . magit-list-submodules)
         ("C-x g i" . magit-init)
         ("C-x g ." . magit-remote-add)))

(provide 'init-magit)
;;; init-magit.el ends here
