;;; modules-package.el --- Package extension -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: pakcage extebsion.


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Package extension

;;; Code:

(defgroup md-pkg nil
  "Package extensions."
  :group 'md-pkg)

(defcustom md-pkg-save-variable-in-custom-file nil
  "Whether to save the installation package variables in custom file."
  :type 'boolean
  :group 'md-pkg)

(defcustom md-pkg-archives 'melpa
  "Set pakcage archives from which to fetch."
  :type '(choice (const :tag "Melpa" melpa)
                 (const :tag "Melpa-mirror" melpa-mirror)
                 (const :tag "Emacs-china" emacs-china)
                 (const :tag "Netease" netease)
                 (const :tag "Tuna" tuna)
                 (const :tag "Tencent" tencent))
  :group 'md-pkg)

;; HACK: Do not save the variable "Package-selected-packages" in custom file
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun md-pkg/save-selected-packages! (&optional value)
  "Set and (don't save `package-selected-packages' to VALUE.)"
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))


;; ELPA: refer to https://melpa.org and https://elpa.emacs-china.org
;;;###autoload
(defun md-pkg/set-package-archives+ (archives)
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

;; ELPA: refer to https://melpa.org and https://elpa.emacs-china.org
;;;###autoload
(defun md-pkg/set-package-archives+ (archives)
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
(defun md-pkg/install+ (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (cond
   ((package-installed-p package min-version)
    t)
   ((or (assoc package package-archive-contents) no-refresh)
    (package-install package))
   (t
    (package-refresh-contents)
    (md-pkg/install+ package min-version t))))

;;;###autoload
(defun md-pkg/initialize! (&optional melpa-archive var-to-custom)
  (let ((var (or var-to-custom md-pkg-save-variable-in-custom-file))
        (melpa-archive (or melpa-archive md-pkg-archives)))
    (unless var
      (advice-add 'package--save-selected-packages
                  :override #'my-save-selected-packages))
    (md-pkg/set-package-archives+ melpa-archive)))

(provide 'modules-package)

;;; modules-package.el ends here
