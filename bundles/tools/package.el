;;; bundles/tools/package.el.el -*- lexical-binding: t -*-


(package! theme-magic
  :if (and IS-LINUX (executable-find "wal"))
  :commands (theme-magic-export-theme-mode theme-magic-from-emacs))

(package! pomidor :commands pomidor)

(package! link-hint :commands (link-hint-copy-link link-hint-open-link))

(package! tldr :commands tldr :if (not IS-WINDOWS))

(package! cnfonts :commands cnfonts-ui :if (display-graphic-p))

(package! esup :commands esup :if (display-graphic-p))
