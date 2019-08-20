;;; iex-ivy.el --- Initialize IVY configurations. -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (ivy)
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

;; IVY SWIPER, counsel
;;(setq lye-use-fuz-or-flx-in-ivy 'fuz)

(pcase lye-use-fuz-or-flx-in-ivy
  ('fuz
   (require-package 'fuz)
   (require 'fuz)
   (unless (require 'fuz-core nil t)
     (fuz-build-and-load-dymod))
   (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
   (setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))

   (with-eval-after-load 'ivy
     (let ((ivy-fuz-file (expand-file-name "ivy-fuz.el" lye-emacs-cache-dir)))
     (unless (file-exists-p ivy-fuz-file)
       (lye/core-require 'core-funcs)
       (download-a-file "https://github.com/cireu/fuz.el/raw/master/ivy-fuz.el" ivy-fuz-file))
     (load ivy-fuz-file :no-error :no-message)
     (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))))

  ('flx
   (require-package 'flx)
   (require 'flx)
   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(lye/modules-require 'iex-amx)

;;; Code:
(require-package 'ivy)
(require-package 'counsel)
(require 'counsel)

(setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)      ; Enable bookmarks and recentf
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function nil)
(setq ivy-display-style 'fancy)

;; @https://github.com/abo-abo/swiper/issues/2130
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)

;; Fuzzy search
;; (if (locate-library "flx"))

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

;; load-locate-key
(lazy-load-set-keys '(([escape] . minibuffer-keyboard-quit)) swiper-map)

(lazy-load-set-keys
 '(("<C-return>" . ivy-immediate-done)
   ([escape]     . minibuffer-keyboard-quit))
 ivy-minibuffer-map)

(ivy-mode 1)
(counsel-mode 1)

(provide 'iex-ivy)

;;; iex-ivy.el ends here
