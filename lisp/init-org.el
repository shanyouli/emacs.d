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
               (auto-fill-mode nil) ; 不自动换行
               (setq truncate-lines nil) ; 不自动换行
               (if (display-graphic-p)
                   (use-package org-bullets
                     :init (org-bullets-mode 1)))
               ;; 自动缩进，* or ** etc..
               (org-indent-mode t)))
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((R . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil)
       (latex . t)
       (ledger . t)
       (ocaml . nil)
       (octave . t)
       (plantuml . t)
       (python . t)
       (ruby . t)
       (screen . nil)
       (,(if (locate-library "ob-sh") 'sh 'shell) . t)
       (sql . t)
       (sqlite . t)))))

(provide 'init-org)
;;; init-org.el ends here
