;;; bundles/snails/package.el.el -*- lexical-binding: t -*-

(package! '(snails :type git
            :host github
            :repo "manateelazycat/snails"
            :no-byte-compile t)
  :commands snails)

(package! '(snails-backend-themes :type git
            :host github
            :repo "shanyouli/snails-backend"
            :no-byte-compile t))
