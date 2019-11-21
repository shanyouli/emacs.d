;;; moudles/apps/sdcv/packages.el -*- lexical-binding: t -*-

(dolist (pkg '((pos-tip)
               (posframe)
               (sdcv :type git
                 :host github
                 :repo "manateelazycat/sdcv"
                 :no-byte-compile t)))
  (package+ pkg))
