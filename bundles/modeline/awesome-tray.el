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
        awesome-tray-module-alist)
  (advice-add 'awesome-tray-disable
              :before (lambda (&rest _) (setq-default mode-line-format lye-init--modeline-format)))
  (advice-add 'awesome-tray-enable
              :before (lambda (&rest _)
                        (let ((first-mode-line (car mode-line-format)))
                          (unless (and (stringp first-mode-line)
                                       (string= " " first-mode-line))
                            (setq-default mode-line-format '(" "))))))

  (add-hook! 'lye-load-theme-hook
    (when awesome-tray-active-p
      (awesome-tray-mode -1)
      (awesome-tray-mode +1))))

;; active-modules
(setq awesome-tray-active-modules
      '("pyim" "location" "parent-dir" "mode-name" "awesome-tab" "date"))

(add-hook (if (daemonp) 'after-make-frame-functions 'lye-init-ui-hook)
          #'awesome-tray-mode 'append)
