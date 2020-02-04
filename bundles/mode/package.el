;;; bundles/mode/package.el -*- lexical-binding: t -*-

;; Json
(package! 'json-mode :mode ("\\.json\\'" . json-mode))

;; Web
(package! 'web-mode :mode (("\\.xml\\'" . web-mode)
                           ("\\fonts.conf\\'" . web-mode)
                           ("\\.html\\'" . web-mode)))

;; Yaml
(package! 'yaml-mode :mode (("\\.yml\\'" . yaml-mode)
                            ("\\.yaml\\'" . yaml-mode)))
;; CSS
(package! 'css-mode :mode ("\\.rasi\\'" . css-mode))

;; markdown GFM
(package! 'markdown-mode :mode ("\\.md\\'" . gfm-mode))
(package! 'markdownfmt :if (executable-find "markdownfmt")
          :commands markdownfmt-format-buffer)

;; VIM-SCRIPT
(package! 'vimrc-mode :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; PKGBUILD
(package! 'pkgbuild-mode :if (executable-find "pacman")
          :mode ("/PKGBUILD\\'" . pkgbuild-mode))

;; EBUILD
(package! 'ebuild-mode :mode ("\\.ebuild\\'" . ebuild-mode)
          :if (executable-find "emerge"))

;; AHK
(package! 'ahk-mode :mode ("\\.ahk\\'" . ahk-mode) :if IS-WINDOWS)

;; POWERSHELL
(package! 'powershell :mode ("\\.ps1\\'" . powershell-mode) :if IS-WINDOWS)

;; PLANTUML
(package! 'plantuml-mode
  :if (executable-find "java"))

;; sh
(package! 'sh-script
  :local t
  :mode (("\\.zsh\\'" . sh-mode)
         (".zshrc" . sh-mode)))

;; LUA
(package! 'lua-mode
  :mode (("\\.lua\\'" . lua-mode)
         (".luacheckrc" . lua-mode)
         ("rc.lua.template" . lua-mode)))
(package! 'company-lua)
(package! '(lsp-lua-emmy :repo "phenix3443/lsp-lua-emmy" :host github)
  :if (executable-find "java"))

;; python
(package! 'yapfify
  :if (executable-find "yapf")
  :commands yapf-mode)
(package! 'pyenv-mode-auto :if (executable-find "pyenv"))
(package! 'live-py-mode :commands live-py-mode)
