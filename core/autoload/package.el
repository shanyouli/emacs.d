;;; core/autoload/package.el -*- lexical-binding: t -*-


;; HACK: DO NOT save the variable "package-selected-packages" in init/custom file
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
;;;###autoload
(defun +save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

;; ELPA: refer to https://melpa.org and https://elpa.emacs-china.org
;;;###autoload
(defun +set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china
                                          netease tencent tuna)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            ('tencent
             `(,(cons "gnu" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
            (archives
             (error "Unknown archives: `%s'" archives)))))

  (message "Set package archives to `%s'." archives))


;; @see https://github.com/redguardtoo/emacs.d/blob/3c54e19d7793e8178b8a357502ae33c62b2db23a/lisp/init-elpa.el#L207
;; On-demand installation of packages
;;;###autoload
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

;;;###autoload
(defun switch-to-straight-buffer ()
  "Open the `*straight-process*'."
  (interactive)
  (let* ((straight-buffer straight-process-buffer)
        (blist (mapcar #'buffer-name (buffer-list))))
    (if (and straight-buffer (member straight-buffer blist))
        (switch-to-buffer straight-buffer))))
