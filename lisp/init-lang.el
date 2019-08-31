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

;;; yasnippet
(use-package yasnippet
  :init
  (with-temp-message
      (with-current-buffer " *Minibuf-0*" (buffer-string))
    (setq-default yas-snippet-dirs (list lye-emacs-yas-snippets-dir))
    (yas-global-mode 1)
    (when (file-exists-p yas--default-user-snippets-dir)
      (delete-directory yas--default-user-snippets-dir)))
  :config
  (use-package yasnippet-snippets))

;;; prettify-mode
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
                ("||" . ?∨)))
(setq prettify-symbols-unprettify-at-point 'right-edge)

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
  :defer t
  :config
  (lye/exec-path-from-shell-init)
  (when (executable-find "markdownfmt")
    (use-package markdownfmt
      :bind (:map markdown-mode-map
             ("C-c f" . markdownfmt-format-buffer)))))

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
  :init   (setq plantuml-default-exec-mode 'jar)
  :config
  (setq plantuml-jar-path lye-emacs-plantuml-file)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar)))

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
