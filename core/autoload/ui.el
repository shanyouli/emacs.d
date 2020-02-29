;;; core/autoload/ui.el -*- lexical-binding: t -*-

;; adjust-opacity https://emacs-china.org/t/topic/2405/22
;;;###autoload
(defun lye//adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame."))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))


;;; Themes
;;;###autoload
(defun lye-theme-list-load (theme)
  "Loading theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (or lye-theme-use-list (custom-available-themes))))))
  (load-theme theme t))

(defun lib-theme--random-load (themes)
  "Pickup random color theme from themes."
  (let ((theme (nth (random (length themes)) themes)))
    (message "Load `%s' theme..." theme)
    (load-theme theme t)))

;;;###autoload
(defun lye-theme-random-load-color ()
  "Random color themes."
  (interactive)
  (lib-theme--random-load (or lye-theme-use-list (custom-available-themes))))
