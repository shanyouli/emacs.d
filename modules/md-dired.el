;;; md-dired.el --- Initialize Module dired -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: dired, hl
;; Last-Updated: 2019-11-20 15:39:28


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

;;  Initialize Module Dired

;;; Change log:
;;
;; 11/20/19

;;; Code:

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies t) ; Recursive copying
  (setq dired-recursive-deletes t) ; Recursive deletion
  (setq dired-listing-switches "-aluh --group-directories-first") ;参数，传递给 ls
  (setq directory-free-space-args "-Pkh") ; 目录空间选项
  (put 'dired-find-alternate-file 'disabled nil))

;; Use asynchronous file management
(use-package dired-async
  :ensure async
  :bind (:map dired-mode-map
         ("C" . dired-async-do-copy)
         ("R" . dired-async-do-rename))
  :hook (dired-mode . dired-async-mode))

;; colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;;Git show modify information
(use-package diff-hl
  :ensure t
  :commands (diff-hl-dired-mode)
  :hook (dired-mode . diff-hl-dired-mode))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :config
  (setq image-dired-dir (concat lye-emacs-cache-dir "image-dired")
        image-dired-thumbnail-storage 'standard))

;; (use-package image-mode :ensure nil)

;; Extra Dired functionality
;; (use-package dired-aux :ensure nil)
(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :defer 0.1
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
         ("H" . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$\\|\\*~$"))
  (let ((cmd (cond
              (IS-WINDOWS "start")
              (IS-LINUX "xdg-open")
              (IS-MAC "open"))))
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

(provide 'md-dired)

;;; md-dired.el ends here
