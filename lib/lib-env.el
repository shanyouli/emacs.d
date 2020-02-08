;;; lib-env.el --- Initialize the environment variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords:env

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize the system environment variables.

;;; Code:
(require 'lib-var)

(defcustom lib-env-from-shell-list '("PATH" "MANPATH")
  "To determine the need to cache environment varialbes!"
  :type 'list)

(defcustom lib-env-path-save-file (expand-file-name "env" user-emacs-directory)
  "Cache environment variables file."
  :type 'file)



(defun lib-env-path-from-shell-getenv-to-string (path)
  "Terminal access and consistent environment variable."
  (let ((shell-executable (format "%s" (getenv "SHELL"))))
    (shell-command-to-string (concat shell-executable " -ic 'echo -n " path "'"))))

(defun lib-env-path-from-shell-getenv-string ()
  "Get environment variables about the element `lib-env-from-shell-list'."
  (let* ((path-string (concat "$" (mapconcat 'identity lib-env-from-shell-list "#..#$")))
         (path-var-string (lib-env-path-from-shell-getenv-to-string path-string)))
    (split-string path-var-string "#..#")))

(defun lib-env-path-from-shell-save (&optional force-p)
  (let ((env-file lib-env-path-save-file))
    (when (or force-p (not (file-exists-p env-file)))
      (let ((path-list lib-env-from-shell-list)
            (path-var-list (lib-env-path-from-shell-getenv-string))
            exec-path-var)
        (with-temp-file env-file
          (insert ";;; -*- mode: emacs-lisp;-*-\n\n"
                  ";;; Here 's where all caches need to use environment variables.\n\n"
                  ";;; Cache environmet variable names have `"
                  (mapconcat 'identity lib-env-from-shell-list "' `")
                  "'\n")
          (while path-list
            (let ((env-element (car path-list))
                  (env-var (lib-var-delete-same-element-in-string (car path-var-list)
                                                                  ":")))
              (insert "\n;;; This is `" env-element "' environment variable.\n"
                      "(setenv \"" env-element  "\" \"" env-var "\")\n")
              (when (string= "PATH" env-element)
                (setq exec-path-var (split-string env-var ":"))))
            (setq path-list (cdr path-list)
                  path-var-list (cdr path-var-list)))
          (insert
           "\n;;; This is `exec-path' environment variable.\n"
           (format "(setq exec-path '%S)" exec-path-var)
           "\n\n" (char-to-string ?\C-l) "\n"
           ";; Local Varibles:\n"
           ";; version-control: never\n"
           ";; no-byte-compile: t\n"
           ";; no-update-autoloads: t\n"
           ";; coding: utf-8\n"
           ";; End:\n"
           (format ";;; %s ends here" (file-name-nondirectory env-file))
           "\n"))))
    (load env-file :no-error :no-message)))

;;;###autoload
(defalias 'lib-env-from-shell-initialize 'lib-env-path-from-shell-save)

;;;###autoload
(defun lib-env/update ()
  "Env-file Update!"
  (interactive)
  (lib-env-path-from-shell-save t))

(provide 'lib-env)
;;; lib-env.el ends here
