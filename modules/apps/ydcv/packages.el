;;; moudles/apps/ydcv/packages.el -*- lexical-binding: t -*-

(dolist (pkg '((youdao-dictionary)
               (popup)
               (pos-tip)
               (chinese-word-at-point)
               (names)
               (posframe)))
  (package+ pkg))
