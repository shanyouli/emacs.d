;;; /core/autoload/frame.el -*- lexical-binding: t -*-

;;;###autoload
(defun +frame-height-golden-section ()
  "The ratio of the default height to the screen height is 0.618."
  (/ (* 618 (x-display-pixel-height)) (* 1000 (frame-char-height))))

;;;###autoload
(defun +frame-height-half ()
  "The ratio of the default height to the screen height is 0.5."
  (- (/ (x-display-pixel-height) (* 2 (frame-char-height))) 2))

;;;###autoload
(defun +frame-widget-golden-section ()
  "The ratio of the default width to the screen width is 0.618."
  (/ (* 618 (x-display-pixel-width)) (* 1000 (frame-char-width))))

;;;###autoload
(defun +frame-width-half ()
  "The ratio of the default width to the screen width is 0.5."
   (- (/ (x-display-pixel-width) (* 2 (frame-char-width))) 2))

;;;###autoload
(defun lye/default-frame-size (&optional frame)
  "Frame default size configuration."
  (interactive)
  (when (display-graphic-p)
    (when frame (select-frame frame))
    (if ( and (boundp system/windows) system/windows)
        (progn
          (set-frame-width (selected-frame) (+frame-width-half))
          (set-frame-height (selected-frame) (+frame-height-golden-section)))
      (set-frame-size (selected-frame)
                      (+frame-width-half)
                      (+frame-height-golden-section)))))
