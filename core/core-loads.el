;;; core/core-loads.el.el -*- lexical-binding: t -*-

;; Load configurations and autoload base file
(eval-when-compile
  (require 'cl-macs)
  (autoload 'cl-remove-if-not "cl-seq"))

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
           (intern (substring  sym-f (length str-prefix)))
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
  (let* ((f (lye//load-file-exist-p feature absolute-path))
         (f-symbol (lye//load-feature-symbol f)))
    `(unless (lye//load-feature-p ',f-symbol)
       (push ',f-symbol lye--load-feature-enable-list)
       (lye//safe-load ,f ,@args))))

;; @see https://github.com/honmaple/maple-emacs/blob/aa8a6896a3bf8146c5dbadcdfc1ae80e99a263eb/lisp/init-basic.el#L59
;;;###autoload
(defmacro lye-add-load-path! (dir &optional subdirp)
  "Add DIR to `load-path',
If SUBDIRP is non-nil, the subdirectory of PATH will add to `load-path'."
  (if subdirp
      `(let ((dirs ',(cl-remove-if-not
                      (lambda (dir) (file-directory-p dir))
                      (directory-files (if (stringp dir)
                                           dir
                                         (symbol-value dir))
                                       t "^[^\\.]"))))
         (setq load-path (append (if dirs dirs (list ,dir)) load-path))
         (cl-pushnew ,dir load-path :test #'string=))
    `(push ,dir load-path)))
