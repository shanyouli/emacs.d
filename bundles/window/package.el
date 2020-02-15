;;; bundles/window/packge.el -*- lexical-binding: t -*-

(pcase lye-use-switch-windows-package
  ('ace-window (package! ace-window :commands ace-window-display-mode))
  ('winum (package! winum :commands winum-mode)))

(package! windmove :build-in t :commands windmove-default-keybindings)

(package! winner  :build-in t :commands (winner-redo winner-undo))

(package! shackle :commands (shackle-display-buffer shackle-mode))
