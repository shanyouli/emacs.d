;;; modules/pyim/config.el -*- lexical-binding: t -*-


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

(defcustom liberime-user-data-dir (lib-f-join lye-emacs-cache-dir "rime/")
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
                          (locate-library "liberime-config")))
         (buf-name "*Liberime Build*"))
    (lye//run-command-with-buf
     "make" buf-name liberime--root)))

(defun liberime--check ()
  (unless (require 'liberime nil t)
    (let ((liberime--module (lib-f-join
                             (file-name-directory
                              (locate-library "liberime-config"))
                             (concat "build/liberime" module-file-suffix))))
      (unless (file-exists-p liberime--module)
        (liberime--build))
      (lye//move-file liberime--module straight-dynamic-modules-dir)
      (lib-safe-load (concat straight-dynamic-modules-dir
                             "liberime.so") nil t))))

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

(require 'pyim)

;; using pyim-dregcache, not use pyim-dhashcache
(setq pyim-dcache-backend 'pyim-dregcache)
;; Using Emacs-async, Emacs thread is nore stagnation than asynchronuous
(setq pyim-prefer-emacs-thread nil)
(setq pyim-dcache-directory (lib-f-join lye-emacs-cache-dir "pyim/dcache"))
(setq default-input-method "pyim")

(with-eval-after-load 'pyim

  ;; Fuzzy pinyin
  ;; (setq pyim-fuzzy-pinyin-alist
  ;;       '(("en" "eng") ("in" "ing") ("l" "n") ("z" "zh") ("c" "ch")
  ;;         ("s" "sh") ("an" "ang")))
  ;; Set 9 Candidate words
  (setq pyim-page-length 9)
  (if (and (display-graphic-p) (require 'posframe nil t))
      (setq pyim-page-tooltip 'posframe
            pyim-posframe-min-width 0)
    (setq pyim-page-tooltip 'minibuffer
          pyim-page-style 'one-line))
  (setq pyim-default-scheme 'quanpin)
  (pcase modules-pyim-use-dict
    ('base (pyim-basedict-enable))
    ('big (pyim-bigdict-enable))
    ('librime (pyim-liberime-enable))))
