;;; bundles/window/packge.el -*- lexical-binding: t -*-

(if (eq lye-use-switch-windows-package 'winum)
    (package! winum :commands winum-mode))
(package! ace-window :commands ace-window-display-mode)

(package! windmove :build-in t :commands windmove-default-keybindings)

(package! winner  :build-in t :commands (winner-redo winner-undo))

(package! shackle :commands (shackle-display-buffer shackle-mode))
