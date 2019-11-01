;;; core/autoload/env.el -*- lexical-binding: t -*-

;;;see@https://emacs-china.org/t/exec-path-from-shell-linux/7942/10?u=shanyouli
;;;see@https://github.com/hlissner/doom-emacs/blob/develop/core/cli/env.el
;;;see@https://github.com/purcell/exec-path-from-shell

;;;###autoload
(defvar env-path-from-shell-list '("PATH")
  "You need to cache the list of environment variables,
`PATH' and `MANPATH' default.")

;;;###autoload
(defun env-path-from-shell-getenv (path)
  (let* ((shell-executable (format "%s" (getenv "SHELL")))
         (shell-get-path (shell-command-to-string
                          (concat shell-executable
                                  " -ic 'printf $"
                                  path
                                  " '"))))
    (format "%s" shell-get-path)))

;;;###autoload
(defun +init-exec-path ()
  (setq exec-path (split-string (getenv "PATH") ":")))

;;;###autoload
(defun env-path-from-shell-getenvs (lst)
  (let ((old-list lst)
        (new-list nil)
        (pathname nil)
        (path-var nil))
  (while old-list
    (setq pathname (car old-list))
    (setq path-var (env-path-from-shell-getenv pathname))
    (setq new-list (cons `(,pathname . ,path-var)
                         new-list))
    (setq old-list (cdr old-list)))
  (nreverse new-list)))

;;;###autoload
(defun env-path-from-shell-setenv (list-name)
  (let* ((list-name (env-path-from-shell-getenvs list-name)))
    (while list-name
      (setenv (caar list-name) (cdar list-name))
      (setq list-name (cdr list-name))))
  (+init-exec-path))

;;;###autoload
(defun env-path-from-shell-save-file (&optional force-p save-file)
  (let ((env-file (if save-file
                      (expand-file-name save-file)
                    lye-emacs-save-env-file)))
    (when (or force-p (not (file-exists-p env-file)))
      (with-temp-file env-file
        (insert
         (concat
          (char-to-string ?\C-l)
          "\n;;;Here 's where all caches need to use environment variables."))

        (let ((list-name (env-path-from-shell-getenvs
                          env-path-from-shell-list)))
          (while list-name
            (insert
             (concat
              "\n\n"
              (char-to-string ?\C-l)
              "\n"
              (format ";;;This is %s environment variable\n (setenv \"%s\" \"%s\")"
                      (caar list-name) (caar list-name) (cdar list-name))))
            (setq list-name (cdr list-name))))

        (insert
         (concat
          "\n\n;;;This is the place value of some environment variables.\n"
          ";;;Now extract environment variables `"
          (substring (format "%s" (prin1 env-path-from-shell-list)) 1 -1)
          "'"))))))

;;;###autoload
(defun lye/force-refresh-env ()
  "Env-file update!"
  (interactive)
  (env-path-from-shell-save-file t)
  (load lye-emacs-save-env-file :no-error :no-message)
  (+init-exec-path))

;;;###autoload
(defun +env-path-from-shell-init ()
  "Initializes the value of an environment variable."
  (env-path-from-shell-save-file)
  (load lye-emacs-save-env-file :no-error :no-message)
  (+init-exec-path))

(+env-path-from-shell-init)
