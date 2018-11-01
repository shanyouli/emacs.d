;;; Package --- Summary

;;; Commentary:

;;; Code:
(require-package 'pyim)
(when (maybe-require-package 'pyim-basedict)
  (require 'pyim)
  (require 'pyim-basedict)
  (add-hook 'pyim-active-hook 'pyim-basedict-enable)
  )
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method )


(if (version< emacs-version "26.0")
    (setq pyim-page-tooltip 'popup)
  (when (maybe-require-package 'posframe)
    (setq pyim-page-tooltip 'posframe)
    ))
(setq pyim-default-scheme 'quanpin
      pyim-page-length 5)


(provide 'init-chinese)
;;; init-chinese.el ends here
