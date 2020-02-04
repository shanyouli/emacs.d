;;; bundles/editor/key.el.el -*- lexical-binding: t -*-


(defonekey thing-edit nil
  "Thing-edit"
  ("w" thing-copy-word "Copy word")
  ("s" thing-copy-symbol "Copy symbol")
  ("f" thing-copy-filename "Copy-filename")
  ("x" thing-copy-sexp "Copy sexp")
  ("g" thing-copy-page "Copy Page")
  ("i" thing-copy-list "copy list")
  ("h" thing-copy-defun "copy functions")
  ("p" thing-copy-parentheses "Copy Parentheses")
  ("l" thing-copy-region-or-line "Copy line or region")
  ("a" thing-copy-to-line-beginning "Copy to Line begin")
  ("e" thing-copy-to-line-end "Copy to Line End")

  ("W" thing-cut-word "Cut word")
  ("S" thing-cut-symbol "Cut symbol")
  ("F" thing-cut-filename "Cut-filename")
  ("X" thing-cut-sexp "Cut sexp")
  ("G" thing-cut-page "Cut Page")
  ("I" thing-cut-list "Cut list")
  ("H" thing-cut-defun "Cut functions")
  ("P" thing-cut-parentheses "Cut Parentheses")
  ("L" thing-cut-region-or-line "Cut line or region")
  ("A" thing-cut-to-line-beginning "Cut to Line begin")
  ("E" thing-cut-to-line-end "Cut to Line End"))
