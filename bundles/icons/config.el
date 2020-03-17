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
  (push '("\\.bash\\'" all-the-icons-fileicon "terminal" :face all-the-icons-pink)
        all-the-icons-icon-alist)
  (push '("\\.webm\\'" all-the-icons-fileicon "video" :face all-the-icons-blue-alt)
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

(with-eval-after-load 'ivy
  (all-the-icons-ivy-rich-mode +1))

(with-eval-after-load 'dired
  (add-hook! 'dired-mode-hook #'all-the-icons-dired-mode)
  (with-eval-after-load 'all-the-icons-dired
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t))
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert " ")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (insert (all-the-icons-icon-for-dir filename nil ""))
                        (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                    ;; Align and keep one space for refeshing after some operations
                    (insert "\t "))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display
                :override #'my-all-the-icons-dired--display)))
