;;; bundles/rime/package.el.el -*- lexical-binding: t -*-

(package! rime :recipe (:type git
                        :host github
                        :repo "DogLooksGood/emacs-rime"
                        :files ("*.el" "Makefile" "lib.c"))
          :custom ((default-input-method "rime")))
