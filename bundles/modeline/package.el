;;; bundles/ui/package.el.el -*- lexical-binding: t -*-

(pcase lye-use-modeline
  ('base t)
  ('doom (package! doom-modeline :commands doom-modeline-mode))
  ('awetray
   (package! awesome-tray :recipe (:type git
               :host github
               :repo "manateelazycat/awesome-tray"
               :no-byte-compile t)
     :commands awesome-tray-mode)
   (package! awesome-tab :recipe (:type git
            :host github
            :repo "manateelazycat/awesome-tab"
            :no-byte-compile t)
     :commands awesome-tab-mode)))
