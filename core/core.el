;;; core.el --- Initialize core -*- lexical-binding: t -*-

;;; Code

(pcase emacs-version
  ((pred (lambda (x) (version< x "27.0")))
   (load (concat user-emacs-directory "early-init") nil 'nomessage))
  ((pred (lambda (x) (version< x "26.1")))
   (error "Detected Emacs %s. Lye-Emacs only supports Emacs 26.1 and higher."
          emacs-version)))

(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(windows-nt ms-doc)))

(defvar lye--initial-file-name-handler-alist file-name-handler-alist)
(defvar lye--initial-exec-path exec-path)

;;
;;; Global variables
(defconst lye-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst lye-core-dir (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of Lye-Emacs's core files. Must end with a slash.")

(defconst lye-core-library-dir (concat lye-core-dir "lib/")
  "The root directory of libray directory. Must end with a slash.")

(defconst lye-etc-dir (concat lye-emacs-dir "etc/")
  "etc dir in `lye-emacs-dir', Must end with a slash.")

(defconst lye-emacs-cache-dir (concat lye-emacs-dir ".cache/")
  "Is the cache directory this?")

;; Ensure `lye-core-dir' and `lye-library-dir'  in `load-path'
(push lye-core-dir load-path)
(push lye-core-library-dir load-path)

;; This is consulted on every `require', `load' and various path/io handling
;; encrypted or compressed files, among other things.
(setq file-name-handler-alist nil)

;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun lye--reset-file-handler-alist-h ()
  (setq file-name-handler-alist lye--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'lye--reset-file-handler-alist-h)

;; Load the bare necessities
(autoload 'lib-f-join "lib-f")
(autoload 'lib-autoload-initialize "lib-autoload")
(autoload 'lye-load! "core-loads")
(autoload 'lye-initialize-base-autoload! "core-loads")

(lye-load! 'core/core-libs)

;; Do this on idle timer to defer a possible GC pause that could result; also
;; allows deferred packages to take advantage of these optimizations.
;; see@https://github.com/emacsmirror/gcmh
(defvar lye--gc-cons-threshold (if (display-graphic-p) #x1000000 #x400000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.
When use graphic, its value is 16Mib, otherwise 4Mib")

(defvar lye--gc-cons-upper-limit (if (display-graphic-p) #x20000000 #x8000000)
  "The temporary value for `gc-cons-threshold' to defer it.
Whe use graphic, its value is 512Mib, otherwise 128Mib.")

(defvar lye--gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

;;
(defvar lye-doctor-p t "Gets the loading time of each library.")

(if lye-doctor-p (require 'lib-doctor))

(add-hook! 'emacs-startup-hook
    (setq gc-cons-threshold lye--gc-cons-threshold)
  ;; GC automatically while unfocusing the frame
  ;; `focus-out-hook' is obsolete since 27.1
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda () (unless (frame-focus-state) (garbage-collect))))
    (add-hook! 'focus-out-hook #'garbage-collect))
  ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  (defun lye-minibuffer-setup-h ()
    (setq gc-cons-threshold lye--gc-cons-upper-limit))
  (defun lye-minibuffer-exit-h ()
    (setq gc-cons-threshold lye--gc-cons-threshold))
  (add-hook! 'minibuffer-setup-hook #'lye-minibuffer-setup-h)
  (add-hook! 'minibuffer-exit-hook #'lye-minibuffer-exit-h))

;;
;;; customization
(defgroup lye nil "Lye-emacs group" :group 'lye)

(defcustom lye-full-name "shanyouli" "Set user full name."
  :type 'string
  :group 'lye)

(defcustom lye-mail-address "shanyouli6@gmail.com"
  "Set user mail address."
  :type 'string
  :group 'lye)

(defconst lye-homepage  "https://github.com/shanyouli/emacs.d"
  "The Github page of My Emacs Configurations.")

(defvar dynamic-module  nil "Save dynamic module dir.")
(defcustom lye-package-dir (concat lye-emacs-dir "pkgs/")
  "The root directory of Lye-emacs's pkg dirs, Must end with a slash."
  :type 'directory
  :set (lambda (symbol value)
         (set symbol value)
         (let* ((emacs-v (format "%s.%s" emacs-major-version emacs-minor-version))
                (package-user-base-name (format "elpa-%s/" emacs-v))
                (dynamic-base-name "dynamic-modules/")
                (straight-build-base-name (format "build-%s/" emacs-v)))
           (setq package-user-dir (lib-f-join value package-user-base-name)
                 straight-base-dir value
                 straight-build-dir (lib-f-join value "straight"
                                                straight-build-base-name)
                 dynamic-module-dir (lib-f-join value dynamic-base-name))))
  :group 'lye)

(mapc (lambda (dir) (unless (file-directory-p dir) (make-directory dir t)))
      (list lye-emacs-cache-dir lye-package-dir dynamic-module-dir))

(define-error 'lye-error "Error in Lye Emacs core")
(define-error 'lye-hook-error "Error in a Doom startup hook" 'lye-error)

;;; Load `custom-file'
(setq custom-file (concat lye-emacs-cache-dir "custom.el"))
(if (file-exists-p custom-file) (load custom-file :no-error :no-message))

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(lye-add-load-path! lye-etc-dir t)
(lye-add-load-path! dynamic-module-dir)
(lye-initialize-base-autoload! lye-core-dir lye-etc-dir)

(defun lye-core-initialize ()
  "Load Lye's core files for an interactive session."
  (lye-load! 'core/core-custom)    ; Custom-Varliable
  (lye-load! 'core/core-generic)   ; generic and delete *scratch*
  (lye-load! 'core/core-straight)  ; staraight, package
  (lye-load! 'core/core-ui)        ; UI
  (lye-load! 'core/core-package)   ; packages initialization
  (lye-load! 'core/core-bundle)
  (lye-load! 'core/core-key)       ; Keybinding
  )
