;;; bundles/window/packge.el -*- lexical-binding: t -*-

(pcase lye-use-switch-windows-package
  ('ace-window (package! 'ace-window :commands ace-window-display-mode))
  ('winum (package! 'winum :commands winum-mode)))

(package! '(windmove :type built-in) :commands windmove-default-keybindings)

(package! '(winner :type built-in) :commands (winner-redo winner-undo))

(package! 'shackle :commands (shackle-display-buffer shackle-mode))
