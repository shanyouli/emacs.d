;;; core/core-straight.el.el -*- lexical-binding: t -*-

;;
;;; package.el

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom lye-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'lye
  :type '(alist :key-type (symbol :tag "Archve grup name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom lye-package-archives 'tuna
  "Set package archives from which to fetch."
  :group 'lye
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value lye-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                                    name)))
                    lye-package-archives-alist)))

(defun lye-set-package-archives (archives &optional refresh async no-save)
  "set the package archives (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list (intern (completing-read "Select package archives: "
                                  (mapcar #'car lye-package-archives-alist)))))
  ;; Set option
  (lye-set-custom-variable 'lye-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))
  (message "Set package archives to `%s'" archives))

;; Refer to https://emacs-china.org/t/elpa/11192
(defun lye-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)
  (let* ((urls (mapcar
                (lambda (url)
                  (concat url "archive-contents"))
                (mapcar #'cdr
                        (mapcar #'cadr
                                (mapcar #'cdr
                                        lye-package-archives-alist)))))
         (durations (mapcar
                     (lambda (url)
                       (let ((start (current-time)))
                         (message "Fetching %s..." url)
                         (cond ((executable-find "curl")
                                (call-process "curl" nil nil nil "--max-time" "10" url))
                               ((executable-find "wget")
                                (call-process "wget" nil nil nil "--timeout=10" url))
                               (t (user-error "curl or wget is not found")))
                         (float-time (time-subtract (current-time) start))))
                     urls))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            lye-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "%s" urls)
    (message "%s" durations)
    (message "%s is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; and `custom-file' and Select the package archives
(when (not (file-exists-p custom-file))
  (if (or (executable-find "curl") (executable-find "wget"))
      (progn
        ;; Get and select the fastest package archives automatically
        (message "Testing connection.. Please wait a moment.")
        (lye-set-package-archives
         (lye-test-package-archives 'no-chart)))
    ;; Select package archives manually
    (lye-set-package-archives
     (intern
      (ido-completing-read "Select package archives:"
                           (mapcar (lambda (x) (symbol-name (car x)))
                                   lye-package-archives-alist))))))

;; (and (file-readable-p custom-file) (load custom-file))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (cond
   ((package-installed-p package min-version)
    t)
   ((or (assoc package package-archive-contents) no-refresh)
    (package-install package))
   (t
    (package-refresh-contents)
    (require-package package min-version t))))
;;
;;; straight.
(defvar straight-core-package-sources
  '((org-elpa :local-repo nil)
    (melpa :type git
           :host github
           :repo "melpa/melpa")
    (gnu-elpa-mirror :type git
                     :host github
                     :repo "emacs-straight/gnu-elpa-mirror")
    (emacsmirror-mirror :type git
                        :host github
                        :repo "emacs-straight/emacsmirror-mirror")))
(defvar lye-builtin-packages '(org
                              pyim
                              async
                              xr
                              pyim-basedict
                              doom-themes
                              noflet
                              super-save
                              posframe
                              restart-emacs
                              which-key
                              straight)
  "Lye-emacs built-in packages.")

;; straight
(setq straight-base-dir lye-emacs-cache-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil  ; use-autoload
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom refresh' instant (once everything set up), which is much
      ;; nicer UX than the several seconds modification checks.
      ;; straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      ;; straight-enable-package-integration nil
      ;; straight-disable-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t
      straight-process-buffer " *straight-process*" ; hide *straight-process*
      straight-check-for-modifications nil)

(defun lye-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* (;; Force straight to install into ~/.emacs.d/package/straight instead of
         ;; ~/.emacs.d/straight by pretending `lye-emacs-package-dir' is our .emacs.d.
         (user-emacs-directory straight-base-dir)
         (bootstrap-file (lib-f-join straight-base-dir "straight"
                                     "repos" "straight.el" "bootstrap.el"))
         (bootstrap-version 5))
    (lib-f-make-dir straight-build-dir)
    (lib-f-make-dir dynamic-module-dir)
    (lye-add-load-path! dynamic-module-dir))
  (require 'straight))
    ;; (unless (featurep 'straight)
    ;;   (unless (or (require 'straight nil t)
    ;;               (file-readable-p bootstrap-file))
    ;;     (with-current-buffer
    ;;         (url-retrieve-synchronously
    ;;          (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
    ;;                  straight-repository-branch)
    ;;          'silent 'inhibit-cookies)
    ;;       (goto-char (point-max))
    ;;       (eval-print-last-sexp)))
    ;;   (load bootstrap-file nil t))))

(with-eval-after-load 'finder-inf
  (setq straight--cached-built-in-packages nil)
  (mapc (lambda (x) (unless (assq x package--builtins)
                 (push `(,x . [nil nil "The package already exists with Lye-Emacs."])
                       package--builtins)))
        lye-builtin-packages))
(defun straight-initialize-packages (&optional force-p)
  "Initialize `package' and `straight',
If FORCE-P are non-nil, do it anyway."
  (message "Initializing straight...")
  (unless (fboundp 'straight--reset-caches)
    (lib-f-make-dir straight-build-dir)
    (lye-ensure-straight)
    (require 'straight)
    ;; (straight--reset-caches)
    ;; (setq straight-recipe-repositories nil
    ;; straight-recipe-overrides nil)
    (mapc #'straight-use-recipes straight-core-package-sources)
    ;; (straight-register-package
    ;;  `(straight :type git :host github
    ;;             :repo ,(format "%s/straight.el" straight-repository-user)
    ;;             :files ("straight*.el")
    ;;             :branch ,straight-repository-branch
    ;;             :no-byte-compile t))
    (mapc (lambda (p) (straight-register-package `(,p :type built-in)))
          lye-builtin-packages)))

(defun switch-to-straight-buffer ()
  "Open the `*straight-process*'."
  (interactive)
  (let* ((straight-buffer straight-process-buffer)
         (blist (mapcar #'buffer-name (buffer-list))))
    (if (and straight-buffer (member straight-buffer blist))
        (switch-to-buffer straight-buffer))))

(defun straight-use-package-a (orig-fun &rest args)
  (let ((pkg-sym (car args)))
    (if (and (symbolp pkg-sym)
             (memq pkg-sym
                   (mapcar #'car
                           (or (bound-and-true-p package-archive-contents)
                               (progn (package-refresh-contents)
                                      package-archive-contents))))
             (ignore-errors (require-package pkg-sym)))
        (straight-register-package `(pkg-sym :type built-in))
      (apply orig-fun args))))
(advice-add #'straight-use-package :around #'straight-use-package-a)

(defvar lye-cache--straight-build-packages nil
  "Use `straight.el' installed packages.")
(defun lye-reload--straight-build-packages ()
  (unless lye-cache--straight-build-packages
    (setq lye-cache--straight-build-packages
          (if (file-directory-p straight-build-dir)
              (mapcar #' intern (directory-files straight-build-dir
                                                 nil "^[^\\.|\\-]"))))))

(defun package-built-in-p-a (orig-fun &rest args)
  (let ((pkg (car args)))
    (lye-reload--straight-build-packages)
    (if  (memq pkg (bound-and-true-p lye-cache--straight-build-packages))
        t
      (apply orig-fun args))))
(advice-add #'package-built-in-p :around #'package-built-in-p-a)

;;
;;; package! functions
(eval-when-compile
  (autoload 'cl-defmacro "cl-macs" nil t)
  (autoload 'bundle-get-path "bundle" nil t))

(cl-defmacro package!
    (name &key disabled if commands mode recipe build-in defer with)
  "Install a package-name.

Usage:

    (package! NAME
        [:keyword [option]])
:if EXPR     Initialize and load only if EXPR evaluates to a non-nil value.
:build-in    If noinstall is t, not run (straight-use-package NAME).
:recipe EXPR when use straight install package, need.
:commands    Define autoloads for commands that that will be defined by the
             package. This is useful if the package is being lazily loaded.
:mode EXPR   run (add-to-list 'auto-mode-alist EXPR).
"
  (declare (indent 1))
  (unless disabled
    (let* ((package (if recipe (cons name recipe) name))
           (bundle with)
           (package-name (symbol-name name))
           (body-lists `,(core-package/concat
                         (package-keys:install package build-in with)
                         (package-keys:commands commands package-name)
                         (package-keys:mode mode package-name)
                         (package-keys:defer defer name))))
      (if if
          (macroexp-progn
           `((when ,if
               ,@(mapcar 'identity body-lists))))
        (macroexp-progn body-lists)))))

(defun package-keys:install (package build-in bundle)
  (unless build-in
    (if bundle
        (let ((bundle-path (bundle-get-path bundle)))
          `((with-eval-after-load ,(expand-file-name "package.el" bundle-path)
              (straight-use-package ',package))))
    `((straight-use-package ',package)))))

(defun package-keys:commands (commands package-name)
  (when commands
    (cl-mapcan
     (lambda (cmd)
       `((unless (fboundp ',cmd) (autoload ',cmd ,package-name nil t))))
     (if (listp commands) commands (list commands)))))

(defun package-keys:mode (mode-alists file)
  (when mode-alists
    (cl-mapcan
     (lambda (alist)
       `((let ((cmd ',(cdr alist)))
           (unless (fboundp cmd) (autoload cmd ,file))
           (add-to-list 'auto-mode-alist ',alist))))
     (if (listp (car mode-alists)) mode-alists (list mode-alists)))))

(defun package-keys:defer (defer package)
  (when defer
    (let ((time (if (numberp defer) defer 0.5)))
      `((run-with-idle-timer
         ,time nil
         (lambda (&rest _) (require ',package nil t)))))))

(defsubst core-package/concat (&rest elem)
  (apply #'append (delete nil (delete (list nil) elem))))

(straight-initialize-packages)
(when (not (bound-and-true-p package--initialized))
  (message "Initializing package.el")
  (package-initialize))
