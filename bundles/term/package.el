;;; bundles/term/package.el -*- lexical-binding: t -*-

(pcase lye-use-term-package
  ('vterm (package! vterm :commands vterm))
  ('multi-term (package! multi-term :commands multi-term)))

(package! shell-pop :commands shell-pop)
