;;; lib-frame.el --- Make Frame Configurations       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shanyouli

;; Author: shanyouli <shanyouli6@gmail.com>
;; Keywords: frames

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

;; Frame Configurations

;;; Code:

(defgroup lib-frame nil "Frame configuration." :group 'lib-frame)

(defcustom lib-frame-height-scale nil
  "The height of window screen ratio accounted for emacs-frame."
  :type 'float
  :group 'lib-frame)

(defcustom lib-frame-width-scale nil
  "The width of window screen ratio accounted for emacs-frame."
  :type 'float
  :group 'lib-frame)



;;;###autoload
(defun lib-frame-setup-height (&optional scale)
  "Setup frame height times the height of the screen SCALE. SCALE defaults to 0.618!"
  (let ((scale (or scale lib-frame-height-scale 0.5))
        (fm-height (/ (x-display-pixel-height) (frame-char-height) 1.0)))
    (if (> scale 1) (/ fm-height scale) (* fm-height scale))))

;;;###autoload
(defun lib-frame-setup-width (&optional scale)
  "Setup frame width times the width of the screen SCALE. SCALE defaults to 0.5!"
  (let ((scale (or scale lib-frame-width-scale 0.5))
        (fm-width (/ (x-display-pixel-width) (frame-char-width) 1.0)))
    (if (> scale 1) (- (/ fm-width scale) 2) (- (* fm-width scale) 2))))

;;;###autoload
(defun lib-frame-default-size (&optional frame width-scale height-scale)
  "The default window size and position."
  (interactive)
  (let* ((x (x-display-pixel-width))
         (y (x-display-pixel-height))
         (fm-width-proportion (or width-scale lib-frame-width-scale 0.5))
         (fm-height-proportion (or height-scale lib-frame-height-scale 0.618))
         (fm-width (* x fm-width-proportion))
         (fm-height (* y fm-height-proportion))
         (fm-x (truncate (/ (- x fm-width) 2)))
         (fm-y (truncate (/ (- y fm-height) 2))))
    (when (display-graphic-p)
      (when frame (select-frame frame))
      (set-frame-position (selected-frame) fm-x fm-y)
      (set-frame-size (selected-frame)
                      (truncate (- fm-width (* 2 (frame-char-width))))
                      (truncate fm-height) t))))

;;;###autoload
(defun lib-frame-get-screen-width ()
  (let* ((edges (frame-edges))
         (width-start (nth 0 edges))
         (width-end (nth 2 edges)))
    (truncate (/ (- width-end width-start) (frame-char-width)))))

(provide 'lib-frame)
;;; lib-frame.el ends here
