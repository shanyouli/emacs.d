;;; moudles/apps/sdcv/packages.el -*- lexical-binding: t -*-

(package! 'pos-tip t)
(package! 'posframe t)
(package! '(sdcv :type git
                 :host github
                 :repo "manateelazycat/sdcv"
                 :no-byte-compile t))
