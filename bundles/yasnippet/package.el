;;; bundles/yasnippet/package.el -*- lexical-binding: t -*-

(package! 'yasnippet :commands yas-global-mode)

(package! 'yasnippet-snippets)

(package! 'ivy-yasnippet :if (eq lye-use-search-frame 'ivy))
