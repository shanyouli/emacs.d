;;; bundles/ui/awesome-tray.el.el -*- lexical-binding: t -*-

(with-eval-after-load 'awesome-tray
  ;; add pyim-modules
  (defface awesome-tray-module-pyim-face
      '((t (:foreground "OrangeRed" :bold t)))
    "Pyim face."
    :group 'awesome-tray)
  (defun awesome-tray-module-pyim-info ()
  "`Pyim' display information in the awesome-tray."
    (if (and (featurep 'pyim) (string= current-input-method "pyim"))
        "<IM>" ""))

  (push '("pyim" . (awesome-tray-module-pyim-info awesome-tray-module-pyim-face))
      awesome-tray-module-alist))

;; active-modules
(setq awesome-tray-active-modules
      '("pyim" "location" "parent-dir" "mode-name" "awesome-tab" "date"))

(defun awesome-tray-initialize+ ()
  "Start `Awesome-tray'"
  (if (fboundp 'theme-switch-light-or-dark-theme)
      (advice-add #'theme-switch-light-or-dark-theme :after #'awesome-tray-mode)
    (awesome-tray-mode +1))
  (when (boundp 'after-load-theme-hook)
    (add-hook 'after-load-theme-hook
              '(lambda ()
                (when (and (boundp 'awesome-tray-active-p) awesome-tray-active-p)
                  (awesome-tray-mode))))))

(add-hook! 'after-init-hook
    (if (display-graphic-p)
        (awesome-tray-initialize+)
      (require 'lib-modeline nil t)))
