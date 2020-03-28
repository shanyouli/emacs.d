;;; bundles/snails/package.el.el -*- lexical-binding: t -*-

(package! snails :recipe (:type git
            :host github
            :repo "manateelazycat/snails"
            :files (:defaults "*.sh" "*.el")
            :no-byte-compile t
            :no-autoloads t)
  :commands snails)

(package! snails-backend-themes :recipe (:type git
            :host github
            :repo "shanyouli/snails-backend"
            :no-byte-compile t))

(package! fuz :commands fuz-build-and-load-dymod
          :if (and (executable-find "cargo") (not IS-WINDOWS)))

;; (package! selectrum :recipe (:host github :repo "raxod502/selectrum")
;;           :commands selectrum-mode)
;; (package! selectrum-prescient
;;           :recipe ( :host github
;;                     :repo "raxod502/prescient.el"
;;                     :files ("selectrum-prescient.el"))
;;   :commands (selectrum-prescient-mode prescient-persist-mode))
