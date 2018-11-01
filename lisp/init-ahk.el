(when (maybe-require-package 'xahk-mode)
  (add-to-list 'auto-mode-alist '("\\.ahk'" .ahk-mode)))

(provide 'init-ahk)
;;;end
