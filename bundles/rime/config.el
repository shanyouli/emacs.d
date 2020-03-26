;;; bundles/rime/config.el.el -*- lexical-binding: t -*-

(setq rime-user-data-dir (lib-f-join lye-emacs-cache-dir "rime/"))

(with-eval-after-load 'rime
  (unless rime-emacs-module-header-root
    (setq-default rime-emacs-module-header-root
                  (cond ((and (= emacs-major-version 27) (= emacs-minor-version 0))
                         "/usr/include/emacs-27-vcs")
                        ((= emacs-major-version 26) "/usr/include/emacs-26")
                        (t "/usr/include/emacs-27"))))
  (when (and (display-graphic-p) (require 'posframe nil t))
    (setq rime-show-candidate 'posframe
          rime-posframe-style (list :background-color "#333333"
                                    :foreground-color "#dcdccc"
                                    :font "Sarasa Mono SC"
                                    :internal-border-width 10)))
  (setq rime-posframe-style 'horizontal))
