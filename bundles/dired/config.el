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
