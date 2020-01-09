;;; bundles/term/package.el -*- lexical-binding: t -*-

(pcase lye-use-term-package
  ('vterm (package+ 'vterm))
  ('multi-term (package+ 'multi-term)))

(package+ 'shell-pop)
