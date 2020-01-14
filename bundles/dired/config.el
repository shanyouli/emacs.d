;;; bundles/dired/config.el -*- lexical-binding: t -*-

(with-eval-after-load 'dired
  (add-hook! 'dired-mode-hook (dired-hide-details-mode +1))
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (when IS-MAC
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and IS-MAC (executable-find "gls"))
            (and (not IS-MAC) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  ;; Colourful dired
  (diredfl-global-mode +1)

  ;; Git show modify information
  (add-hook! 'dired-mode-hook (diff-hl-dired-mode +1))

  ;; Shows icons
  (add-hook! 'dired-mode-hook (all-the-icons-dired-mode +1)
    :if (bundle-active-p 'icons))
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
                :override #'my-all-the-icons-dired--display))

  ;; Extra Dired functionality
  (add-hook! 'dired-mode-hook (dired-omit-mode +1))
  (with-eval-after-load 'dired-x
    (setq dired-omit-verbose nil
          dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
    (let ((cmd (cond (IS-WINDOWS "start")
                     (IS-LINUX "xdg-open")
                     (IS-MAC "open"))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))))
