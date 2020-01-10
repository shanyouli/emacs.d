;;; /core/core-bundle.el -*- lexical-binding: t -*-


(bundle! pyim :defer t)

(bundle! term :defer t)

(bundle! fcitx :defer t :if (and IS-LINUX (executable-find "fcitx-remote")))
