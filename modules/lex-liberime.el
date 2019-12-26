;;; lex-liberime.el --- PinYin Input Method -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (pyim liberrime)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords:PinYin


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

;; Use the librime library as the backend for pyim

;;; Code:

(defconst librime-share-dir
  (cond
   ((and IS-LINUX (file-directory-p "/usr/share/rime-data"))
    (file-truename "/usr/share/rime-data"))
   ((and IS-MAC
         (file-directory-p "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
    (file-truename "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")))
  "Personal data storage location.")

(defconst liberime-default-custom-file
  (expand-file-name "rime/default.custom.yaml" lye-emacs-share-dir)
  "The default configuration definition file `liberime'")

(defcustom liberime-user-dir
  (if pyim-dcache-directory
      (expand-file-name "rime" pyim-dcache-directory)
    (expand-file-name "pyim/rime" lye-emacs-cache-dir))
  "一个目录, 用于保存 rime配置目录."
  :type 'directory)

(defvar use-liberime-p nil
  "Whether Liberime-module has been loaded.")

(defun librime-status-p ()
  "Determine if the rime input method is installed."
  (if librime-share-dir t nil))

(defun liberime-load-p (&optional root-dir)
  "Import the liberime module."
  (if (fboundp 'module-load)
      (let* ((liberime--root
              (or root-dir
                  (file-name-directory (locate-library "liberime-config"))))
             (liberime--module
              (concat liberime--root "build/liberime" module-file-suffix)))

        (lye/modules-require 'lex-pyim)

        (unless (and liberime--module (featurep 'liberime))
          (load liberime--module :no-error :no-message))

        (unless (file-directory-p liberime-user-dir)
          (make-directory liberime-user-dir t))
        (let ((cf (expand-file-name "default.custom.yaml" liberime-user-dir)))
          (unless (file-exists-p cf)
            (copy-file liberime-default-custom-file cf)))
        t)
    nil))

(defun liberime-initialize (&optional user-dir)
  "Use `liberime' module."
  (interactive)
  (if (librime-status-p)
      (if (not use-liberime-p)
          (if (liberime-load-p)
              (if (featurep 'liberime)
                  (let ((liberime-user-data-dir (or user-dir
                                                    liberime-user-dir)))
                    (liberime-start librime-share-dir liberime-user-data-dir)
                    ;; (liberime-select-schema "luna_pinyin_simp")
                    (liberime-select-schema "pinyin_simp")
                    (setq pyim-default-scheme 'rime-quanpin)
                    (setq use-liberime-p t))
                (message "liberime does not exist."))

            (message "The emacs currently used does not support dynamic modules.. Please recompile and install emacs with `--with-modules'."))

        (message "Liberime module has been loaded."))

    (message "Librime does not exist.")))

(provide 'lex-liberime)

;;; lex-liberime.el ends here
