;;; core/core-straight.el.el -*- lexical-binding: t -*-

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

(defvar lye-build-in-packags
  '((org :type built-in)
    (pyim :type built-in)
    (async :type built-in)
    (xr :type built-in)
    (pyim-basedict :type built-in)
    (doom-themes :type built-in)
    (noflet :type built-in)
    (super-save :type built-in)
    (posframe :type built-in)
    (restart-emacs :type built-in)
    (which-key :type built-in)))

(defvar lye-core-packages '(straight)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

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
      straight-check-for-modifications nil
      straight-build-dir (concat straight-base-dir "straight/build")
      straight-dynamic-modules-dir (concat straight-base-dir "dynamic-modules/"))

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
    (lib-f-make-dir straight-dynamic-modules-dir)
    (lye-add-load-path! straight-dynamic-modules-dir)
    (unless (featurep 'straight)
      (unless (or (require 'straight nil t)
                  (file-readable-p bootstrap-file))
        (with-current-buffer
            (url-retrieve-synchronously
             (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
                     straight-repository-branch)
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil t))))

(defun straight-initialize-packages (&optional straight-init-notp)
  "Initialize `package' and `straight',
If STRAIGHT-INIT-NOTP are non-nil, then `straight.el' is not initialized."
  (unless straight-init-notp
    (message "Initializing straight...")
    (unless (fboundp 'straight--reset-caches)
      (lye-ensure-straight)
      (require 'straight))
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
    (mapc #'straight-use-package lye-build-in-packags)
    (mapc #'straight-use-package lye-core-packages)))

(defun switch-to-straight-buffer ()
  "Open the `*straight-process*'."
  (interactive)
  (let* ((straight-buffer straight-process-buffer)
         (blist (mapcar #'buffer-name (buffer-list))))
    (if (and straight-buffer (member straight-buffer blist))
        (switch-to-buffer straight-buffer))))
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
