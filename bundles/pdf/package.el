;;; bundles/pdf/package.el.el -*- lexical-binding: t -*-

(package! pdf-tools
  :commands (pdf-tools-install pdf-view-mode pdf-view-midnight-minor-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(package! nov :commands nov-mode :mode ("\\.epub\\'" . nov-mode))
