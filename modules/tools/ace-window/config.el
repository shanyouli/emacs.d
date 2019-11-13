;;;###autoload
(custom-set-faces '(aw-mode-line-face
                    ((t (:inherit mode-line-emphasis :bold t)))))
(custom-set-faces '(aw-leading-char-face
                    ((t (:inherit font-lock-keyword-face :bold t :height 2.5)))))

(add-hook 'emacs-startup-hook #'ace-window-display-mode)
