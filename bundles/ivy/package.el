;;; bundles/ivy/package.el -*- lexical-binding: t -*-

(package! 'counsel)
(package! 'ivy)
(package! 'swiper)
(package! 'amx)

(package! 'ivy-rich :if (and (display-graphic-p) (not IS-WINDOWS)))
