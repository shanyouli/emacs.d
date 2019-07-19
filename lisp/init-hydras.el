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
(require 'hydra)

;; Nice looking hydras
(require 'pretty-hydra )
(defun pretty-hydra-title (title &optional icon-type icon-name face)
    "Pretty hydra title."
    (let ((title-face (or face 'all-the-icons-blue)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face title-face :v-adjust 0.0))
              " "))))
       (propertize title 'face title-face))))

;; test and base
(pretty-hydra-define toggles-hydra
  (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
   :color amaranth :quit-key "q")
  ("Basic"
   (("n" display-line-numbers-mode "Line number" :toggle t))
   "Highlight"
   (("l" hl-line-mode "Line" :toggle t))
   "Coding"
   (("S" prettify-symbols-mode "pretty symbol" :toggle t)
    ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

;; (use-package pretty-hydra
;;   :ensure nil
;;   :demand
;;   :bind ("<f6>" . toggles-hydra/body)
;;   :config
;;   (defun pretty-hydra-title (title &optional icon-type icon-name face)
;;     "Pretty hydra title."
;;     (let ((title-face (or face 'all-the-icons-blue)))
;;       (concat
;;        (when (and (display-graphic-p) icon-type icon-name)
;;          (let ((f (intern (format "all-the-icons-%s" icon-type))))
;;            (when (fboundp f)
;;              (concat
;;               (apply f (list icon-name :face title-face :v-adjust 0.0))
;;               " "))))
;;        (propertize title 'face title-face))))

;;   ;; test and base
;;   (pretty-hydra-define toggles-hydra
;;     (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
;;      :color amaranth :quit-key "q")
;;     ("Basic"
;;      (("n" display-line-numbers-mode "Line number" :toggle t))
;;      "Highlight"
;;      (("l" hl-line-mode "Line" :toggle t))
;;      "Coding"
;;      (("S" prettify-symbols-mode "pretty symbol" :toggle t)
;;       ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
;;       ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))))))

(provide 'init-hydras)

;;; init-hydras.el ends here
