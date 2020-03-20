;;; core/core-loads.el.el -*- lexical-binding: t -*-

;; Load configurations and autoload base file
(eval-when-compile
  (require 'cl-macs)
  (autoload 'cl-remove-if-not "cl-seq")
  (require 'lib-var)
  (require 'lib-f))

(defvar lye--load-feature-enable-list nil
  "Stores whether a particular feature is provided alread.")

(defun lye//load-feature-p (feature)
  "Determine whether a file is loaded load."
  (let ((feature-symbol (lye//load-feature-symbol feature)))
    (memq feature-symbol lye--load-feature-enable-list)))

;;;###autoload
(defun lye//safe-load (file &optional noerror nomessage)
  "Load FILE and don't error out. ARGS is as same as in `load'."
  (condition-case err
      (load file noerror nomessage)
    (error (message (format "Error occured:\n%s\n"
                            (error-message-string err))))))

(defun lye//load-feature-symbol (feature)
  "FEATURE can be a symbol, a string. Return symbol."
  (pcase feature
    ((pred symbolp) feature)
    ((pred stringp)
     (let ((sym-f (file-name-sans-extension feature))
           (str-prefix (or lye-emacs-dir user-emacs-directory)))
       (if (string-prefix-p str-prefix sym-f)
           (intern (substring sym-f (length str-prefix)))
         (intern sym-f))))
    (_ (error "Cannot make into `%s' symbol." feature))))

(defun lye//load-file-exist-p (feature &optional absolute-path)
  "Determine whether a file exists."
  (let* ((base-name (pcase feature
                      ((pred stringp) feature)
                      ((pred symbolp) (symbol-name feature))
                      ((pred listp)  (symbol-name (cadr feature)))))
         (base-file (concat base-name ".el")))
    (if (file-exists-p base-file)
        (expand-file-name base-file)
      (if (and absolute-path (file-directory-p absolute-path))
          (expand-file-name base-file absolute-path)
        (cl-loop for dir in (list
                             (file-name-directory (or load-file-name buffer-file-name))
                             (or lye-emacs-dir user-emacs-directory))
                 when (file-exists-p (concat dir base-file))
                 return (concat dir base-file))))))

;;;###autoload
(defmacro lye-load! (feature &optional absolute-path &rest args)
  "Load a file (use `(lye//load-file-exist-p feature &aptional absolute-path)'
get it)."
  (declare (indent defun))
  (let* ((f (lye//load-file-exist-p feature absolute-path))
         (f-symbol (lye//load-feature-symbol f)))
    (if f
      `(unless (lye//load-feature-p ',f-symbol)
         (push ',f-symbol lye--load-feature-enable-list)
         (lye//safe-load ,f ,@args)))))

;; @see https://github.com/honmaple/maple-emacs/blob/aa8a6896a3bf8146c5dbadcdfc1ae80e99a263eb/lisp/init-basic.el#L59
;;;###autoload
(defmacro lye-add-load-path! (dir &optional subdirp)
  "Add DIR to `load-path',
If SUBDIRP is non-nil, the subdirectory of PATH will add to `load-path'."
  (declare (indent defun))
  (if subdirp
      `(let ((dirs ',(cl-remove-if-not
                      (lambda (dir) (file-directory-p dir))
                      (directory-files (if (stringp dir)
                                           dir
                                         (eval dir))
                                       t "^[^\\.]"))))
         (setq load-path (append (if dirs (cons  ,dir dirs) (list ,dir))
                                 load-path)))
    `(push ,dir load-path)))

;;; Autoload
;; 将所有自己配置的文件的 autoload 功能提取到一个文件, 并设置好 `load-path' 变量

(defcustom lye-autoload-save-file
  (expand-file-name "base-pkg.el" (or lye-emacs-cache-dir user-emacs-directory))
  "autoload function before saving download plug-ins, and `load-path'."
  :type 'file
  :group 'lye)

(defvar lye--autoload-directory-list nil
  "Needs to be performed for determining whether `lye/load-lye-autoload' again.")

(defun lye//autoload-save-variable-to-custom (dirs)
  "Set and save `lye--autoload-directory-list' in `custom-file'."
  (if after-init-time
      (let ((save-silently inhibit-message)))
    (add-hook 'after-init-hook #'lye//autoload-save-variable-to-custom)))

(defun lye//autoload-generate-update-file (dirlist target &optional force)

  (let ((generated-autoload-file (file-name-base target))
        (dir-list (if (listp dirlist) dirlist (list dirlist))))
    (when (or force (not (file-exists-p target)))
      (require 'autoload)
      (let ((target-parent (file-name-directory target)))
        (unless (file-directory-p target-parent)
          (make-directory target-parent t)))
      (if after-init-time
          (let ((save-silently inhibit-message))
            (customize-save-variable 'lye--autoload-directory-list
                                     dir-list))
        (add-hook 'after-init-hook
                  (lambda ()
                    (let ((save-silently inhibit-message))
                      (customize-save-variable 'lye--autoload-directory-list
                                               dir-list)))))
      (with-temp-file target
        (insert
         (concat
          ";; ----------------------------------------------------------------------------\n"
          ";;        Lye Emacs base `autoload' and `load-path' configuration\n"
          ";; ----------------------------------------------------------------------------\n"
          ";; This file has been generated by Lye-Emacs. It contains `load-path' and \n"
          ";; generated-autoload-code."
          ";; \n"
          ";; Please change the file. When You change some file in `lye-emacs-dir' and\n"
          ";; Update Lye-emacs, please run `lye/force-update-autoload'.\n"
          ";; ----------------------------------------------------------------------------\n"
          "\n"
          ";; `load-path' variables:\n"
          ";; ----------------------\n"))
        (dolist (dir dir-list)
          (setq dir (if (symbolp dir) (symbol-value dir) dir))
          (dolist (f (lib-f-list-subfile dir))
            (let ((generated-autoload-load-name (file-name-sans-extension f)))
              (autoload-generate-file-autoloads f (current-buffer)))))
        (insert (concat "\n"
                        (char-to-string ?\C-l)
                        "\n"
                        ";; Local Varibles:\n"
                        ";; version-control: never\n"
                        ;; ";; no-byte-compile: t"
                        ";; no-update-autoloads: t\n"
                        ";; coding: utf-8\n"
                        ";; End:\n"
                        (format ";;; %s ends here" (file-name-nondirectory target))
                        "\n")))
      (byte-compile-file target))
    (load (file-name-sans-extension target) t t)))

;;;###autoload
(defmacro lye-initialize-base-autoload! (&rest args)
  (declare (indent defun))
  `(let ((dir-lists ',(mapcar (lambda (x) (if (symbolp x) (symbol-value x) x))
                              (copy-tree args)))
         (target lye-autoload-save-file)
         force)
     (unless (lib-var-list-eql lye--autoload-directory-list dir-lists)
       (setq force t))
     (lye//autoload-generate-update-file dir-lists target force)))

;;;###autoload
(defun lye/force-update-autoload ()
  (interactive)
  (let ((dir-list lye--autoload-directory-list)
        (target lye-autoload-save-file))
    (if lye--autoload-directory-list
        (lye//autoload-generate-update-file dir-list target t)
      (message "`lye--autoload-directory-list' is nil, Please run
`(lye-initialize-base-autoload! DIR1 DIR2..)' initialize it..")
      (lye//autoload-generate-update-file dir-list target t))))
