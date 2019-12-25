;;; iex-ivy.el --- Initialize IVY configurations. -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.5
;; Package-Requires: (amx ivy counsel swiper ivy-yasnippet :optional fuz flx)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: search


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

;;; v0.5
;; Formatted file structure
;; Time 2019.09.02 21:23

;;; Code:

;;; Must package
(require 'amx)
(require 'ivy)
(require 'counsel)
(require 'swiper)
;; -----------------------------------------------------------------------------

;;; AMX
(let ((smex-file (expand-file-name "smex-items" lye-emacs-cache-dir)))
  (if (file-exists-p smex-file)
      (setq amx-save-file smex-file)
    (setq amx-save-file (concat lye-emacs-cache-dir "amx-items"))))

(setq amx-history-length 10)
(with-eval-after-load 'counsel (amx-initialize))
;; -----------------------------------------------------------------------------

;;; Fuzzy-Match
(pcase lye-use-fuz-or-flx-in-ivy
  ('fuz
   (lye/modules-require 'iex-fuz)
   (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
   (setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))

   (with-eval-after-load 'ivy
     (require 'ivy-fuz)
     (add-to-list 'ivy-highlight-functions-alist
                  '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn))))

  ('flx
   (require 'flx)
   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))
;; -----------------------------------------------------------------------------

;;; counsel, swiper and ivy
(setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)      ; Enable bookmarks and recentf
(setq ivy-height 8)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function nil)
(setq ivy-display-style 'fancy)

;; @https://github.com/abo-abo/swiper/issues/2130
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)

(when (locate-library "magit")
  (setq magit-completing-read-function 'ivy-completing-read))

;; Use faster search tools: rigprep
(let ((command
       (cond
        ((executable-find "rg")
         "rg -i M 120 --no-beading --line-number --color never '%s' %s"))))
  (setq counsel-grep-base-command command))

;; swiper
(setq swiper-action-recenter t)

;; helpful
(when (locate-library "helpful")
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(ivy-mode +1)

(provide 'iex-ivy)

;;; iex-ivy.el ends here
