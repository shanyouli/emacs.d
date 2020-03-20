;;; core/autoload/funs.el.el -*- lexical-binding: t -*-

;; Dos2Unix/Unix2Dos
;;;###autoload
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
;;;###autoload
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
;; (bind-keys ("C-M-r" . revert-current-buffer))

;; Save a file as utf-8
;;;###autoload
(defun save-buffer-as-utf-8 (coding-system)
  "Revert a buffer with 'CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system 'utf-8)
  (save-buffer))

;; Save a file as gbk
;;;###autoload
(defun save-buffer-with-gbk (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as gbk."
  (interactive "zcoding system for visited file (default nil):")
  (revert-buffer-with-coding-system 'gbk)
  (save-buffer))

;; Revert a buffer as gbk
;;;###autoload
(defun revert-buffer-with-gbk ()
  "Reopen the buffer in gbk format."
  (interactive)
  (revert-buffer-with-coding-system 'gbk))

;; Revert a buffer as utf-8
;;;###autoload
(defun revert-buffer-with-utf-8 ()
  "Reopen the buffer in utf-8 format."
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))

;; Recompile elpa directory
;;;###autoload
(defun recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir nil)
    (byte-recompile-directory package-user-dir nil t)))

;; Rename the current file
;;;###autoload
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-file-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; Browse current HTML file
;;;###autoload
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-fle-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;Edit files with root privileges
;; see @https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/files.el
;;;###autoload
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@"
                         (file-remote-p 'host) "|sudo:root@"
                         (file-remote-p file 'host) ":"
                         (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))

;;;###autoload
(defun erase-all-buffer ()
  (interactive)
  (delete-region (point-min) (point-max)))
