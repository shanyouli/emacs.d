;;; bundles/pyim/liberime.el -*- lexical-binding: t -*-

;;
;;; liberime
(defcustom liberime-shared-data-dir
  (pcase system-type
    ('gnu/linux
     (cl-some (lambda (parent)
                (let ((dir (lib-f-join parent "rime-data")))
                  (when (file-directory-p dir)
                    dir)))
              '("/usr/share/local" "/usr/share")))
    ('darwin "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
  "Data directory on the system."
  :type 'directory)

(defcustom liberime-user-data-dir
  (or (cl-some (lambda (parent)
                 (let ((dir (lib-f-join parent "rime")))
                   (when (file-directory-p dir)
                     dir)))
               (list (concat (getenv "XDG_CONFIG_HOME") "/fcitx")
                     (concat (getenv "HOME") "/.config/fcitx")
                     (concat (getenv "XDG_CONFIG_HOME") "/ibus")
                     (concat (getenv "HOME") "/.config/ibus")))
      (lib-f-join lye-emacs-cache-dir "rime/"))
  "Data directory on the user home directory."
  :type 'directory)

(defcustom liberime-default-custom-file
  (lib-f-join liberime-user-data-dir "default.custom.yaml")
  "Save default custom with rime customize."
  :type 'file)
(defcustom liberime-use-schema "luna_pinyin_simp"
  "Select a schema."
  :type 'string)

(defun liberime--build ()
  (let ((liberime--root (file-name-directory
                          (locate-library "liberime")))
         (buf-name "*Liberime Build*"))
    (lye//run-command-with-buf
     "make" buf-name liberime--root)))

(defun liberime--check ()
  (unless (require 'liberime-core nil t)
    (let ((liberime--module (lib-f-join
                             (file-name-directory
                              (locate-library "liberime"))
                             (concat "build/liberime-core" module-file-suffix))))
      (unless (file-exists-p liberime--module)
        (liberime--build))
      (lye//move-file liberime--module straight-dynamic-modules-dir)
      (lye//safe-load (concat straight-dynamic-modules-dir
                              "liberime-core.so") nil t))))

(defun liberime--check-shared-data-p ()
  (if (and liberime-shared-data-dir
           (file-directory-p liberime-shared-data-dir)
           liberime-user-data-dir
           (file-directory-p liberime-user-data-dir))
      t
    (user-error "Plase set `liberime-shared-data-dir' or `liberime-user-data-dir'")))

(defun pyim-liberime-enable ()
  (liberime--check)
  (unless (file-exists-p liberime-default-custom-file)
    (let* ((root--dir (file-name-directory (or load-file-name buffer-file-name)))
           (default-file (lib-f-join root--dir "default.custom.yaml")))
      (lib-f-make-parent-dir liberime-default-custom-file)
      (copy-file default-file liberime-default-custom-file)))
  (liberime--check-shared-data-p)
  (liberime-start liberime-shared-data-dir liberime-user-data-dir)
  (liberime-select-schema liberime-use-schema)
  (setq pyim-default-scheme 'rime-quanpin))
