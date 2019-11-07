;;; modules-env.el --- Initialize the environment variables -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: Env


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

;; Initialize the system environment variables.
;; reference:
;;           https://emacs-china.org/t/exec-path-from-shell-linux/7942/10
;;           https://github.com/hlissner/doom-emacs/blob/develop/core/cli/env.el
;;           https://github.com/purcell/exec-path-from-shell

;;; Code:

(defgroup modules-env nil
  "Environment Variables Module."
  :group 'modules-env)

(defcustom mde-path-from-shell-list '("PATH" "MANPATH")
  "To determine the need to cache environment variables!"
  :type 'list
  :group 'modules-env)

(defcustom mde-path-from-shell-save-file
  (expand-file-name "env-path" (if (boundp 'lye-emacs-cache-dir)
                              lye-emacs-cache-dir
                            user-emacs-directory))
  "Cache environment variables file."
  :type 'file
  :group 'modules-env)

(defun mde/shell-list-to-string+ ()
  "Conversion `mde-path-from-shell-list' is a string!"
  (let ((path-list mde-path-from-shell-list)
        (path-str))
    (while path-list
      (setq path-str (concat path-str (format "${%s}#..#" (car path-list))))
      (setq path-list (cdr path-list)))
    (format "%s" path-str)))

(defun mde/path-from-shell-getenv-to-string (path)
  (let* ((shell-executable (format "%s" (getenv "SHELL")))
         (shell-get-path (shell-command-to-string
                          (concat shell-executable
                                  " -ic 'printf "
                                  path
                                  " '"))))
    (format "%s" shell-get-path)))

;;;###autoload
(defun mde/path-from-shell-save-file+ (&optional force-p save-file)
  (let ((env-file (if save-file
                      (expand-file-name save-file)
                    mde-path-from-shell-save-file)))
    (if (or force-p
            (not (file-exists-p env-file)))
        (with-temp-file env-file
          (insert
           (concat ";;; -*- mode: emacs-lisp;-*-\n\n"
                   ";;; Here 's where all caches need to use environment variables.\n\n"

                   (format ";;; Cache environment variable names have `%s'.\n"
                           (substring (format "%s"  mde-path-from-shell-list) 1 -1))))

          (let* ((path-string (mde/path-from-shell-getenv-to-string
                               (mde/shell-list-to-string+)))
                 (path-var-list (split-string path-string "#..#"))
                 (path-list mde-path-from-shell-list))
            (while path-list
              (insert
               (concat "\n\n"
                       (char-to-string ?\C-l)
                       "\n"
                       (format ";;;This is %s environment variable\n(setenv \"%s\" \"%s\")"
                               (car path-list) (car path-list) (car path-var-list))))
              (setq path-var-list (cdr path-var-list))
              (setq path-list (cdr path-list))))
          (insert (concat "\n\n" (char-to-string ?\C-l) "\n"
                          ";; Local Varibles:\n"
                          ";; version-control: never\n"
                          ";; no-byte-compile: t\n"
                          ";; no-update-autoloads: t\n"
                          ";; coding: utf-8\n"
                          ";; End:\n"
                          (format ";;; %s ends here"
                                  (file-name-nondirectory env-file))
                          "\n"))))))

;;;###autoload
(defun mde/force-refresh-env ()
  "Env-file update!"
  (interactive)
  (mde/path-from-shell-save-file+ t)
  (load mde-path-from-shell-save-file :no-error :no-message)
  (setq exec-path (split-string (getenv "PATH") ":")))

;;;###autoload
(defun mde/path-from-shell-initialize+ ()
  "Initializes the value of an environment variable."
  (mde/path-from-shell-save-file+)
  (load mde-path-from-shell-save-file :no-error :no-message)
  (setq exec-path (split-string (getenv "PATH") ":")))


(provide 'modules-env)

;;; modules-env.el ends here
