;;; bundles/snails/package.el.el -*- lexical-binding: t -*-

(package! snails :recipe (:type git
            :host github
            :repo "manateelazycat/snails"
            :no-byte-compile t)
  :commands snails)

(package! snails-backend-themes :recipe (:type git
            :host github
            :repo "shanyouli/snails-backend"
            :no-byte-compile t))

(package! fuz :commands fuz-build-and-load-dymod
          :if (and (executable-find "cargo") (not IS-WINDOWS)))

(package! smex :commands (smex-initialize smex smex-major-mode-commands))

(package! ido-sort-mtime :commands ido-sort-mtime-mode)
