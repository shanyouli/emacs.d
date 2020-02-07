;;; bundles/git/package.el.el -*- lexical-binding: t -*-


(package! 'magit :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
                        ("\\MERGE_MSG\\'" . text-mode))
          :commands (magit-status magit-dispatch magit-file-popup))

(package! 'magit-todos :commands magit-todos-mode)

(package! 'forge :if (executable-find "cc"))
