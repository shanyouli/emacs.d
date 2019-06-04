;;; init-windows.el ---Window Configurations.        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gmail.com>
;; Keywords:

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
(add-hook 'after-init-hook #'(lambda () (winner-mode 1)))
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

(defvar lye-ration-dict
  '((1 . 1.618033988875)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 0.618803398875))
  "The ratio dictionary.")

(defun lye/split-window-horizontally (&optional ratio)
  "Split window horizontally and resize the new window.
'C-u number M-x lye/split-window-horizontally' uses pre-defined ration from
`lye-ration-dict'. Always focus on bigger window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ration-val (cdr (assoc ratio lye-ration-dict)))
      (split-window-horizontally (floor (/ (window-body-width)
                                           (1+ ratio-val)))))
     (t
      (split-window-horizontally)))
    (set-window-buffer (next-window) (current-buffer))
    (if (or (not ratio-val)
            (>= ration-val 1))
        (windmove-right))))

(defun lye/split-window-vertically (&optional ratio)
  "Split window vertically and resize the new window.
'C-u number M-x lye/split-window-vertically' uses pre-defined ratio from
`lye/ratio-dict'. Always focus on bigger window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ratio-val (cdr (assoc ratio lye-ration-dict)))
      (split-window-vertically (floor (/ (window-body-height)))))
     (t
      (split-window-vertically)))
    ;; open another window with current-buffer
    (setq-window-buffer (next-window) (current-buffer))
    ;; move focus if new window bigger than current one
    (if (or (not ratio-val)
            (>= ratio-val 1))
        (windmove-down))))

(global-set-key (kbd "C-x 2") 'lye/split-window-vertically)
(global-set-key (kbd "C-x 3") 'lye/split-window-horizontally)

(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :hook (after-init . ace-window-display-mode))

(provide 'init-window)
;;; init-windows.el ends here
