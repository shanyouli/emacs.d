;;; init-org.el ---Org-mode configurations.          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>
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

(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
     ("C-c l" . org-store-link)
     ("C-c c" . org-capture)
     ("C-c b" . org-switchb))
  :mode ("\\.org$\\'" . org-mode)
  :ensure nil
  :hook (org-mode . (lambda ()
                      ;; (auto-fill-mode nil) ; 不自动换行
                      (setq truncate-lines nil) ; 不自动换行

                      ;; Beautify Org Checkbox Symbol
                      ;; @seehttps://github.com/seagle0128/.emacs.d/blob/master/lisp/init-org.el#L44
                      (when (char-displayable-p ?☐)
                        (push '("[ ]" . "☐") prettify-symbols-alist))
                      (when (char-displayable-p ?☑)
                        (push '("[X]" . "☑") prettify-symbols-alist))
                      (when (char-displayable-p ?❍)
                        (push '("[-]" . "❍") prettify-symbols-alist))
                      (when (char-displayable-p ?λ)
                        (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
                        (push '("#+END_SRC" . "λ") prettify-symbols-alist))
                      (prettify-symbols-mode)))
  :init (require 'doom-themes-ext-org)
  )

;; Prettify UI
(use-package org-bullets
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◯" "◉" "◌" "⚈" "☉" "✥" "✡" "✤" "✣" "✴")))

;; 自动缩进，* or ** etc..
(use-package org-indent
  :ensure nil
  :hook ((org-mode . org-indent-mode)
         (org-indent . (lambda ()
                         ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                         (make-variable-buffer-local 'show-paren-mode)
                         (setq show-paren-mode nil)))))


;;; org-babel
;; (with-eval-after-load 'org
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      `((R . t)

;;        (emacs-lisp . t)
;;        (haskell . nil)

;;        (,(if (locate-library "ob-sh") 'sh 'shell) . t)
;;        (sql . t)
;;        (sqlite . t))))

(use-package org-babel
  :defer t
  :ensure nil
  :init
  ;; Don't ask to eval code in SRC blocks.
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t))

(provide 'init-org)
;;; init-org.el ends here
