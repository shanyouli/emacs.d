;;; init-edit.el ---Initialize Edit Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye liivy-alt-done <shanyouli6@gamil.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Do not use the mouse in the graphical interface
(when (display-graphic-p)
  (use-package disable-mouse
    :hook ((after-init after-change-major-mode) . disable-mouse-mode))
  ;; (use-package disable-mouse :hook  (after-init . global-disable-mouse-mode))
  )

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Pair Automatic completion
(use-package autopair
  :diminish autopair-mode
  :hook (after-init . autopair-global-mode))
(use-package rainbow-delimiters
  :hook (autopair-mode . rainbow-delimiters-mode))

;; Chinese input automatically adds spaces in Chinese
;; (use-package pangu-spacing
;;   :diminish pangu-spacing-mode
;;   :hook (after-init . global-pangu-spacing-mode))

;; Big blank delete
(use-package hungry-delete
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode))

;; Show native line numbers if possible, otherwise use linum
(if (and (display-graphic-p) (and (fboundp 'lye/frame-default-width)
                                  (< (lye/frame-default-width) 86)))
    (global-display-line-numbers-mode -1)
  (if (fboundp 'display-line-numbers-mode)
      (use-package display-line-numbers
        :ensure nil
        :hook (prog-mode . display-line-numbers-mode)
        :config
        (setq line-number-display-limit large-file-warning-threshold)
        (setq line-number-display-limit-width 1000))
    (use-package linum-off
      :demand
      :defines linum-format
      :hook (after-init . global-linum-mode)
      :config
      (setq linum-format "%4d "))))

;; Set blank highlight when use display graphic
(if  (display-graphic-p)
    (use-package highlight-indent-guides
      :hook (prog-mode . highlight-indent-guides-mode)
      :config
      (setq highlight-indent-guides-method 'character)
      (setq highlight-indent-guides-responsive t)))

;; add color display
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))

;; Extra blank hint
(use-package whitespace
  :ensure nil
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(provide 'init-edit)
;;; init-edit.el ends here
