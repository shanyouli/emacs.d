;;; modules/UI/awesome-tray/autoload.el -*- lexical-binding: t -*-

;;; add pyim-modules

;;;###autoload
(defface awesome-tray-module-pyim-face
  '((t (:foreground "OrangeRed" :bold t)))
  "Pyim face."
  :group 'awesome-tray)

;;;###autoload
(defun awesome-tray-module-pyim-info ()
  "`Pyim' display information in the awesome-tray."
  (if (and (featurep 'pyim) (string= current-input-method "pyim"))
      "<IM>" ""))

;;;###autoload
(defun awesome-tray-initialize+ ()
  "Start `Awesome-tray'"
  (if (fboundp 'theme-switch-light-or-dark-theme)
      (advice-add #'theme-switch-light-or-dark-theme :after #'awesome-tray-mode)
    (awesome-tray-mode))
  (when (boundp 'after-load-theme-hook)
    (add-hook 'after-load-theme-hook
              '(lambda () (when (and (boundp 'awesome-tray-active-p) awesome-tray-active-p)
                       (awesome-tray-mode))))))
