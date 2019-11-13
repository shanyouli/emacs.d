;;; moudles/apps/sdcv/packages.el -*- lexical-binding: t -*-

(package! 'pos-tip nil t)
(package! 'posframe nil t)
(package! '(sdcv :type git
                 :host github
                 :repo "manateelazycat/sdcv"
                 :no-byte-compile t))
