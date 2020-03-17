;;; bundles/eaf/config.el.el -*- lexical-binding: t -*-

(setq eaf-config-location (expand-file-name "eaf" lye-emacs-cache-dir))

(setq eaf-proxy-type "socks5")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "1080")

(defun syl-eaf-open (path &optional _linkstr)
  (if (file-directory-p path)
      (syl-system-open path)
    (funcall 'eaf-open path)))

;; @see https://emacs-china.org/t/topic/6199/713
(defun syl-system-open (path &optional _linkstr)
  (cond (IS-WINDOWS
          (w32-shell-execute "open" path))
         (IS-MAC
          (concat "open " (shell-quote-argument path)))
         (IS-LINUX
          (let ((process-connection-type nil))
            (start-process "" nil "xdg-open" path)))))

(defun syl-find-file (orig-fun &rest args)
  (let ((filename (car args))
        (cmd (symbol-name this-command)))
    (cond ((cl-find-if
            (lambda (regexp)
              (string-match regexp filename))
            '("\\.pdf\\'" "\\.png\\'" "\\.jpe?g\\'" "\\.bmp\\'" "\\.gif\\'" "\\.mp4\\'" "\\.mkv\\'" "\\.flv\\'"))
           (syl-eaf-open filename))
          ((cl-find-if
            (lambda (regexp)
              (string-match regexp filename))
            '("\\.docx?\\'" "\\.xlsx?\\'" "\\.pptx?\\'" "\\.wps?\\'"))
           (if (executable-find "libreoffice")
               (syl-eaf-open filename)
             (syl-system-open filename)))
          ((and (or (string-match "^org-" cmd))
                (file-directory-p filename))
           (syl-system-open filename))
          (t (apply orig-fun args)))))

(dolist (f '(find-file
             find-file-read-only
             find-file-other-frame
             find-file-other-window
             org-open-file))
  (advice-add f :around 'syl-find-file))
