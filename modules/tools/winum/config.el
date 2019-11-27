;;; modules/tools/winum/config.el -*- lexical-binding: t -*-

(require 'winum)
(winum-mode)
(md-key/set-local
 '(("M-0" . winum-select-window-0-or-10)
   ("M-1" . winum-select-window-1)
   ("M-2" . winum-select-window-2)
   ("M-3" . winum-select-window-3)
   ("M-4" . winum-select-window-4))
 winum-keymap)
