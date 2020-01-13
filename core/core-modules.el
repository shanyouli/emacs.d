;;; core-modules.el --- Initialize modules -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: modules
;; Last-Updated: 2019-11-20 11:06:33


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

;; `lye-modules-dir'初始化

;;; Change log:
;;
;; 11/20/19

;;; Code:

(defcustom lye-modules-package-list '()
  "`lye-modules-dir' 中需要的 packages"
  :type 'list)

(setq lye-modules-package-list
      '(
        ;; iex-avy
        avy
        ace-pinyin
        ;; iex-flycheck
        flycheck
        flycheck-posframe
        ;; iex-fuz
        fuz
        ;; iex-git
        magit
        ;; ivy-fuz
        flx
        ivy-yasnippet
        ;; iex-lsp
        lsp-mode
        lsp-ui
        company-lsp
        ;; iex-pomidor
        pomidor
        ;; iex-pretty-hydra
        pretty-hydra
        ;; iex-smart-align
        (smart-align :type git :host github :repo "manateelazycat/smart-align"
                     :no-byte-compile t)
        ;; iex-snails
        (snails :type git
                :host github
                :repo "manateelazycat/snails"
                :no-byte-compile t)
        (snails-backend-themes :type git
                               :host github
                               :repo "shanyouli/snails-backend"
                               :no-byte-compile t)
        ;;iex-treemacs
        treemacs
        treemacs-projectile
        treemacs-magit
        ;; lex-hydra
        hydra
        ;; lex-ido
        ido-completing-read+
        smex
        flx-ido
        ido-sort-mtime
        ;; md-company
        company
        company-tabnine
        company-posframe
        ;; md-dired
        diff-hl
        diredfl
        ;; md-edit
        rainbow-delimiters
        hungry-delete
        highlight-indent-guides
        rainbow-mode
        page-break-lines
        ;; md-hugo
        ox-hugo
        easy-hugo
        ;;
        link-hint
        org-cliplink
        ;;
        yasnippet
        yasnippet-snippets
        ;;
        cnfonts
        ;; iex-awetab
        (awesome-tab :type git
                     :host github
                     :repo "manateelazycat/awesome-tab"
                     :no-byte-compile t)
        ;; iex-awetray
        (awesome-tray :type git
                      :host github
                      :repo "manateelazycat/awesome-tray"
                      :no-byte-compile t)
        ;; iex-doom-modeline
        doom-modeline
        ;; md-org
        org-bullets
        ob-go
        ob-rust
        ob-ipython
        htmlize))
(mapc #'package+ lye-modules-package-list)

(package+ 'pdf-tools)

;;
;;; python-mode
(defvar lye-lsp-python-ms-p nil
  "当为 t 时, 使用 `lsp-python-ms-p'.")

(if (executable-find "yapf")
    (package+ 'yapf))
(when (executable-find "pyenv")
  (package+ 'pyenv-mode)
  (package+ 'pyenv-mode-auto))
(package+ 'live-py-mode)

(when lye-lsp-python-ms-p
  (package+ 'lsp-python-ms))

;;
;;; md-lua
(package+ 'lua-mode)
(package+ 'company-lua)
(package! '(lsp-lua-emmy :repo "phenix3443/lsp-lua-emmy" :host github))

;;
;;;
(package+ 'nov)
(package+ 'web-mode)
(package+ 'yaml-mode)
(package+ 'json-mode)
(package+ 'markdown-mode)
(package+ 'pkgbuild-mode)
(package+ 'plantuml-mode)
(package+ 'powershell)
(package+ 'vimrc-mode)
(when (executable-find "emerge")
  (package+ 'ebuild-mode))

(package+ 'cmuscheme)
(package+ 'paredit)

(and IS-LINUX (package+ 'theme-magic))

(and IS-WINDOWS (package+ 'ahk-mode))

(provide 'core-modules)

;;; core-modules.el ends here
