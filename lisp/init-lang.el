;;; init-lang.el ---Some Program Language Initialize  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords: languages

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

;; exec-path config
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure nil
    :init
    (require 'cache-path-from-shell)
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;;; yasnippet
(use-package yasnippet
  :init (yas-global-mode 1)
  :config (use-package yasnippet-snippets))

;;; prettify-mode
(use-package prettify-symbols-mode
  :ensure nil
  :init
  (setq-default prettify-symbols-alist
                '(
                  ("<-" . "←")
                  ("->" . ?→)
                  ("->>" . ?↠)
                  ("=>" . ?⇒)
                  ("map" . ?↦)
                  ("/=" . ?≠)
                  ("!=" . ?≠)
                  ("==" . ?≡)
                  ("<=" . ?≤)
                  (">=" . ?≥)
                  ("=<<" . (?= (Br . Bl) ?≪))
                  (">>=" . (?≫ (Br . Bl) ?=))
                  ("<=<" . ?↢)
                  (">=>" . ?↣)
                  ("&&" . ?∧)
                  ("||" . ?∨)
                  ("not" . ?¬)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;;; Program Languages

;; json
(use-package json-mode :mode "\\.json\\'"   :defer t)

;; xml
(use-package web-mode
  :mode (("\\.xml\\'" . web-mode)
         ("\\fonts.conf\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :defer t)

;; yaml
(use-package yaml-mode
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :defer t)

;; markdown, md
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode))
  :defer t)

;; sh-mode
(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
         (".zshrc" . sh-mode))
  ;;:hook (sh-mode . (lambda () (sh-set-shell "bash")))
  )
;; vimrc-major mode
(use-package vimrc-mode :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; PKGBUILD-mode
(use-package pkgbuild-mode
  :if (executable-find "pacman")
  :ensure t
  :mode (("/PKGBUILD\\'" . pkgbuild-mode))
  :defer t)

;; plantuml
(use-package plantuml-mode
  :config
  (when (file-exists-p (concat user-emacs-directory "plantuml/plantuml.jar"))
    (setq plantuml-jar-path
          (concat user-emacs-directory "plantuml/plantuml.jar"))))

;; Only suitable for Windows Languages-Packages major-mode
(when (and (boundp system/windows) system/windows)
  ;; ahk-mode
  (use-package ahk-mode :mode "\\.ahk\\'"  :defer t))

(when system/windows
  ;; powershell-mode
  (use-package powershell
    :mode ("\\.ps1\\'" . powershell-mode)
    :defer t
    :init
    (setq explicit-shell-file-name
          "C:\\Windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
    ;; interactive, but no command prompt
    (setq explicit-powershell.exe-args '("-Command" "-"))))


(provide 'init-lang)
;;; init-lang.el ends here
