;;@see https://emacs-china.org/t/mode-line/374
;;@see https://emacs-china.org/t/mode-line-window/6852
(defun lye/simplify-major-mode-name ()
  "Return simplifyed major mode name"
  (let* ((major-name (format-mode-line "%m"))
         (replace-table '(Emacs-Lisp "𝝀"
                                     Python "Py"
                                     Shell ">"
                                     Markdown "𝓜"
                                     Org "𝒪"
                                     Text "𝓣"
                                     Fundamental "ℱ"))
         (replace-name (plist-get replace-table (intern major-name))))
    (if replace-name replace-name major-name
        )))

(setq-default mode-line-format
              (list
               ""
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
               "(" ;;%02 to set to 2 chars at  least; prevents flickering
               ;;"%02l" "," "02c"
               (propertize "%02l" 'face  'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "
               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-consstant-face) ;;% above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "
               ;; the current major mode for the buffer.
               "["
               '(:eval (propertize (lye/simplify-major-mode-name) 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               ":"
               '(:eval (format "%s" buffer-file-coding-system))
               "] "

               "["
               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face nil
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))


               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face nil
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face nil
                                                  'help-echo "Buffer is read-only"))))
               "] "

               ;;global-mode-string, org-timer-set-timer in org-mode need this
               (propertize "%M" 'face nil)
               '(:eval (if  (>= (window-width) 100)
                           (progn (setq display-time-day-and-date t)
                                  (setq display-time-default-load-average nil)
                                  (display-time-mode t))
                         (progn (setq display-time-day-and-date nil)
                                (setq display-time-default-load-average nil)
                                (display-time-mode t))))
               " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'

               ))

(provide 'init-modeline)
