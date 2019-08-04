;;; init-hydras.el --- Initialize hydras -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1.0
;; Package-Requires: (hydras use-package pretty-hydra)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: key


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package hydra
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra-call-interacctively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map))

(use-package pretty-hydra
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

;; test and base
;; (pretty-hydra-define toggles-hydra
;;   (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
;;    :color amaranth :quit-key "q")
;;   ("Basic"
;;    (("n" display-line-numbers-mode "Line number" :toggle t))
;;    "Highlight"
;;    (("l" hl-line-mode "Line" :toggle t))
;;    "Coding"
;;    (("S" prettify-symbols-mode "pretty symbol" :toggle t)
;;     ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
;;     ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

;; (when (locate-library "setup-font")
;;   (pretty-hydra-define font-size-menu
;;     (:title (pretty-hydra-title "Change font size" 'faicon "font")
;;      :color amaranth :quit-key "q")
;;     ("Inc"
;;      (("=" increase-setup-font-size "Inc font size"))
;;      "Dec"
;;      (("-" decrease-setup-font-size "Dec font size"))
;;      "default"
;;      (("0" default-setup-font-size "Default font size")))))

(provide 'init-hydras)

;;; init-hydras.el ends here
