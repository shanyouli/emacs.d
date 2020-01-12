;;; /core/core-bundle.el -*- lexical-binding: t -*-


(bundle! pyim :defer t)
;; (bundle! fcitx :defer t :if (and IS-LINUX (executable-find "fcitx-remote")))

(bundle! term :defer t)

(bundle! icons :if (and (display-graphic-p) (not IS-WINDOWS)))

(bundle! dict :commands lye/dict-point)
