(when (maybe-require-package 'which-key)
  (require 'which-key)
  (add-hook 'after-init-hook 'which-key-mode))

(provide 'init-which-key)
