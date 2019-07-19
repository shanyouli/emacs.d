;;; core-third-package.el --- Initialize third-packages -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli
;; Keywords:


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

;;; Code:

;; Use undo-tree
(use-package undo-tree
  :ensure nil
  :commands global-undo-tree-mode
  :hook (after-init . global-undo-tree-mode))
;; (setq undo-tree-history-directory-alist
;;       `(("." . ,(concat lye-emacs-cache-dir "undo"))))

;; Save Emacs buffers when they lose focus after 1.5s
(use-package auto-save
  :ensure nil
  :defines (auto-save-silent auto-save-idle)
  :commands (auto-save-enable)
  :init (setq auto-save-silent t
              auto-save-idle 1.5)
  :hook (after-init . auto-save-enable))

;; Displays the key bindings following your currently entered incomplete command
(use-package which-key
  :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :init (setq which-key-idle-delay 0.3))

(provide 'core-third-package)

;;; core-third-package.el ends here
