;;; bundles/window/ace-window.el -*- lexical-binding: t -*-

(face-spec-set 'aw-mode-line-face
               '((t (:inherit mode-line-emphasis :bold t :underline t))))
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit font-lock-keyword-face :bold t :height 2.0 :box t)))))

(add-hook! 'emacs-startup-hook (ace-window-display-mode +1))
