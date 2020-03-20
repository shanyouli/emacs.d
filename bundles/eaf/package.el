;;; bundles/eaf/package.el.el -*- lexical-binding: t -*-

(package! eaf :recipe (:type git :host github
                       :repo "manateelazycat/emacs-application-framework"
                       :files ("app" "core" "*.el" "*.py")
                       :no-byte-compile t)
          :commands (eaf-open eaf-open-url eaf-open-rss))
