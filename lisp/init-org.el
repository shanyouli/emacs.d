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
(require 'cl)

(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :mode ("\\.org$\\'" . org-mode)
  :ensure nil
  :hook (org-mode .
                  (lambda ()
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

(add-hook 'org-mode-hook
          (lambda ()
            (when (featurep 'company)
              (setq-local company-backends
                          (remove-if (lambda (x) (eq 'company-tabnine (car x)))
                                     company-backends)))))

(use-package org-babel
  :defer t
  :ensure nil
  :init
  ;; Don't ask to eval code in SRC blocks.
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  (if (>= emacs-major-version 26)
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))

(provide 'init-org)
;;; init-org.el ends here
