;;; modules-frame.el --- Initialize Frame size -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: Frame


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

;; Frame size

;;; Code:

(defgroup modules-frame nil
  "Frame configuration module."
  :group 'modules-frame)

(defcustom md-frame-height-scale nil
  "The height of window screen ratio accounted for emacs-frame."
  :type 'float
  :group 'modules-frame)

(defcustom md-frame-width-scale nil
  "The width of window screen ratio accounted for emacs-frame."
  :type 'float
  :group 'modules-frame)

;;;###autoload
(defun md/frame-height-set+ (&optional scale)
  "Set frame height times the height of the screen `scale',
`scale' defaults to 0.618."
  (let* ((scale (or scale  md-frame-height-scale 0.618))
         (fm-height (/ (x-display-pixel-height) (frame-char-height))))
    (if (> scale 1)
        (truncate (/ fm-height scale))
      (truncate (* fm-height scale)))))

;;;###autoload
(defun md/frame-width-set+ (&optional scale)
  "Set frame width times the height of the screen `scale',
`scale' defaults to 0.5."
  (let ((scale (or scale md-frame-width-scale 0.5))
        (fm-width (/ (x-display-pixel-width) (frame-char-width))))
    (if (> scale 1)
        (-  (truncate (/ fm-width scale)) 2)
      (-  (truncate (* fm-width scale)) 2))))

;;;###autoload
(defun md/frame-default-size (&optional frame width-scale height-scale)
  "The default window size and position."
  (interactive)

  (let* ((x (x-display-pixel-width))
         (y (x-display-pixel-height))
         (fm-width-proportion (or width-scale md-frame-width-scale 0.5))
         (fm-height-proportion (or height-scale md-frame-height-scale 0.618))
         (fm-width (* (md/frame-width-set+ fm-width-proportion) (frame-char-width)))
         (fm-height (truncate (* fm-height-proportion y)))
         (frame-x (- (/ (- x fm-width) 2) (frame-char-width)))
         (frame-y (/ (- y fm-height) 2)))
    (message "%s" fm-width)
    (when (display-graphic-p)
      (when frame (select-frame frame))
      (set-frame-position (selected-frame) frame-x frame-y)
      (if (memq system-type '(windows-nt ms-doc cygwin))
          (progn
            (set-frame-width (selected-frame) fm-width nil t)
            (set-frame-height (selected-frame) fm-height nil t)))
      (set-frame-size (selected-frame) fm-width fm-height t))))

;;;###autoload
(defun md/frame-size-after-make-frame-func+ (frame)
  (md/frame-default-size frame))

(provide 'modules-frame)

;;; modules-frame.el ends here
