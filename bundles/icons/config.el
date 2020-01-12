;;; bundles/icons/config.el -*- lexical-binding: t -*-

(with-eval-after-load 'all-the-icons
  ;;
  ;;; icons
  (push '("\\(\\.\\|\\)conf\\(ig\\|\\)\\'" all-the-icons-fileicon "config"
          :height 1.0 :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.lua\\(\\.template\\|\\)\\'" all-the-icons-fileicon  "lua"
          :height 1.0 :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.mkv\\'" all-the-icons-faicon "film" :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.rasi\\'" all-the-icons-alltheicon "css3" :face all-the-icons-blue)
        all-the-icons-icon-alist)
  (push '("\\.ebuild\\'" all-the-icons-fileicon "gentoo" :face all-the-icons-purple)
        all-the-icons-icon-alist)
  ;;
  ;;; mode icons
  (push '(vterm-mode all-the-icons-faicon "terminal" :face all-the-icons-yellow)
        all-the-icons-mode-icon-alist)
  (push '(conf-mode all-the-icons-fileicon "config" :face all-the-icons-blue)
        all-the-icons-mode-icon-alist)

  (push '(conf-unix-mode all-the-icons-fileicon "config" :face all-the-icons-yellow)
        all-the-icons-mode-icon-alist)
  (push '(ebuild-mode all-the-icons-fileicon "gentoo" :face all-the-icons-purple)
        all-the-icons-mode-icon-alist)
  ;;
  ;;; dir-icons
  (push '("Videos?" all-the-icons-faicon "film") all-the-icons-dir-icon-alist)
  (push '("Work" all-the-icons-material "work") all-the-icons-dir-icon-alist))

;; all-the-icons-dired
(add-hook! 'dired-mode-hook :if (and (display-graphic-p) (not IS-WINDOWS))
  #'all-the-icons-dired-mode)
