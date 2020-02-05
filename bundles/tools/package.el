;;; bundles/tools/package.el.el -*- lexical-binding: t -*-


(package! 'theme-magic
  :if (and IS-LINUX (executable-find "wal"))
  :commands (theme-magic-export-theme-mode theme-magic-from-emacs))
