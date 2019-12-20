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
        (avy)
        (ace-pinyin)
        ;; iex-elfeed
        elfeed
        ;; iex-exec-path.el
        (exec-path-from-shell)
        (cache-path-from-shell :repo "manateelazycat/cache-path-from-shell"
                               :host github
                               :type git)
        ;; iex-flycheck
        (flycheck)
        (flycheck-posframe)
        ;; iex-fuz
        fuz
        ;; iex-git
        (magit)
        ;; iex-ivy.el
        (amx)
        (ivy)
        (counsel)
        (swiper)
        ivy-fuz
        (flx)
        (ivy-yasnippet)
        ;; iex-lsp
        (lsp-mode)
        (lsp-ui)
        (company-lsp)
        ;; iex-paradox
        paradox
        ;; iex-pomidor
        pomidor
        ;; iex-pretty-hydra
        (pretty-hydra)
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
        (treemacs)
        (treemacs-projectile)
        (treemacs-magit)
        ;; lex-hydra
        (hydra)
        ;; lex-ido
        (ido-completing-read+)
        (smex)
        (flx-ido)
        (ido-sort-mtime)
        ;; md-company
        (company)
        (company-tabnine)
        (company-posframe)
        ;; md-dired
        (async)
        (diff-hl)
        (diredfl)
        (all-the-icons-dired)
        ;; md-edit
        (rainbow-delimiters)
        (hungry-delete)
        (highlight-indent-guides)
        (rainbow-mode)
        (page-break-lines)
        ;; md-elisp
        macrostep
        elisp-refs
        (elispfl :type git :host github
                 :repo "cireu/elispfl")
        (sly-el-indent :type git
                       :host github
                       :repo "cireu/sly-el-indent"
                       :files (:defaults "lib")
                       :no-byte-compile t)
        ;; md-hugo
        (ox-hugo)
        ;;md-lua
        (lua-mode :type git :host github
                  :repo "immerrr/lua-mode")
        (company-lua)
        ;;
        (link-hint)
        (org-cliplink)
        ;;
        (yasnippet)
        (yasnippet-snippets)
        ;; iex-winum
        (winum)
        ;;
        (cnfonts)
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
        (doom-modeline)))
(dolist (md-package lye-modules-package-list)
  (package+ md-package))

;;
;;; iex-fcitx
(when (and IS-LINUX
           (or (executable-find "fcitx-remote")
               (executable-find "fcitx5-remote")))
  (package+ 'fcitx)
  (run-with-idle-timer! :defer 2
    (if (lye-is-running-p "fcitx")
        (lye/modules-require 'iex-fcitx))))

;;
;;; lpm-install, vterm,
(defvar lye-package--use-vterm  nil)
(if (and module-file-suffix
           (executable-find "make")
           (executable-find "libtool")
           (executable-find "cmake"))
    (progn
      (package+ 'vterm)
      (setq lye-package--use-vterm t))
  (package+ 'multi-term))
(package+ 'shell-pop)

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

(if (executable-find "sdcv")
    (package+ '(sdcv :repo "manateelazycat/sdcv"
                :host github
                :type git))
  (package+ 'youdao-dictionary))

;;
;;; windows
(package+ 'ace-window)
(run-with-idle-timer! :defer 2
  ;; window-move
  (lye/modules-require 'iex-ace-window))

;;
;;; md-lua
(package+ '(lua-mode))
(package+ '(company-lua))
(package+ '(lsp-lua-emmy :repo "phenix3443/lsp-lua-emmy"
            :host github
            :type git))

;; (run-with-idle-timer 5 nil (lambda () (md/autoload-create-and-load '(straight-build-dir . "straight"))))

(provide 'core-modules)

;;; core-modules.el ends here
