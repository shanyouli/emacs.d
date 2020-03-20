;;; bundles/ivy/package.el -*- lexical-binding: t -*-

(package! counsel)
(package! ivy)
(package! swiper)
(package! amx)

(package! ivy-rich :commands ivy-rich-mode)

(package! prescient :commands prescient-persist-mode)
(package! ivy-prescient :commands ivy-prescient-re-builder)
(require 'prescient)
