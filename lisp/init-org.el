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
  :config
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; (auto-fill-mode nil) ; 不自动换行
               (setq truncate-lines nil) ; 不自动换行
               ;; 自动缩进，* or ** etc..
               (org-indent-mode t)))

  (add-to-list 'org-src-lang-modes '("plantuml" . puml)))

;; Prettify UI
(use-package org-bullets
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode))

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
  (setq org-confirm-babel-evaluate nil))

;; ob-python
(use-package ob-python
  :defer t
  :ensure nil
  :commands (org-babel-execute:python))

;; ob-shell
(use-package ob-shell
  :defer t
  :ensure nil
  :commands
  (org-babel-excute:sh
   org-babel-expand-body:sh

   org-babel-excute:bash
   org-babel-expand-body:bash))

;; ob-emacs-lisp
(use-package ob-emacs-lisp
  :ensure nil
  :defer t
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-plantuml
  :ensure nil
  :defer t
  :commands (org-babel-execute:plantuml)
  :config
  (if (file-exists-p (concat user-emacs-directory "plantuml.jar"))
      (setq org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
    (setq org-plantuml-jar-path "~/plantuml.jar")))

(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path (concat user-emacs-directory "plantuml.jar")))

(provide 'init-org)
;;; init-org.el ends here
