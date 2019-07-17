;;; init-dired.el --- Initialize Dired               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:

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

;;

;;; Code:
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("H" . dired-omit-mode)
              ("RET" . dired-find-alternate-file)
              ("C-c C-e" . wdired-change-to-wdired-mode))
  ;; see @https://stackoverflow.com/questions/95631/open-a-file-with-su-sudo-inside-emacs
  :hook (dired-mode .
                    (lambda ()
                      ;; open current file as sudo
                      (local-set-key
                       (kbd "C-x <M-S-return>")
                       (lambda()
                         (interactive)
                         (message "!!! SUDO opening %s" (dired-file-name-at-point))
                         (lye/sudo-find-file (dired-file-name-at-point))))))
  :config
  (setq dired-recursive-copies t) ; Recursive copying
  (setq dired-recursive-deletes t) ; Recursive deletion
  (setq dired-listing-switches "-aluh --group-directories-first") ;参数，传递给 ls
  (setq directory-free-space-args "-Pkh") ; 目录空间选项
  (put 'dired-find-alternate-file 'disabled nil))

;; Use asynchronous file management
(use-package dired-async
  :ensure async
  :diminish dired-async-mode
  :hook (dired-mode-hook . dired-async-mode))

(use-package all-the-icons-dired
  :if (and (display-graphic-p) (not system/windows))
  :hook (dired-mode . all-the-icons-dired-mode))

;; colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :config
  (setq image-dired-dir (concat maple-cache-directory "image-dired")
        image-dired-thumbnail-storage 'standard))

(use-package image-mode :ensure nil)

;; Extra Dired functionality
(use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :demand
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$\\|\\*~$"))
  (let ((cmd (cond
              (system/windows "start")
              (system/linux "xdg-open")
              (system/mac "open"))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

;; Music player(Need external commands `mpc' and `mpd')
(when (and (executable-find "mpc") (executable-find "mpd"))
  (use-package simple-mpc
    :ensure t
    :commands (simple-mpc simple-mpc-next simple-mpc-prev simple-mpc-toggle
                        simple-mpc-delete simple-mpc-query )))

(provide 'init-dired)
;;; init-dired.el ends here
