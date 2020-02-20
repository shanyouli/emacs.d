;;; core/autoload/ui.el -*- lexical-binding: t -*-

;;;###autoload
(defun lye-font-initialize ()
  "Font initalization."
  (when (display-graphic-p)
    (require 'lib-font)
    (setq lib-font-english (or lye-en-font
                               (lib-font-exist-p "Fantasque Sans Mono")
                               (lib-font-exist-p "Fira Code"))
          lib-font-chinese (or lye-zh-font
                               (and IS-LINUX "WenQuanYi Micro Hei")
                               (and IS-MAC "Hiragio Sans GB")
                               (and IS-WINDOWS "Microsoft Yahei"))
          lib-font-default-size (or lye-default-font-size 14))
    (lib-font-initialize-monospace)))

;;;###autoload
(defun lye|font-initialize (frame)
  (with-selected-frame frame (lye-font-initialize)))

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

;;
;;; theme

;;;###autoload
(defun lye|initialize-theme (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (setq lib-theme-list lye-theme-list)
    (if (or lye-autoload-switch-dark-or-light-p
            lye-autoload-switch-theme-and-time)
        (progn
          (require 'lib-themes)
          (setq lib-theme-switch (car lye-autoload-switch-theme-and-time)
                lib-theme-switch-time (cdr lye-autoload-switch-theme-and-time))
          (lib-theme-switch-theme))
      (when lye-default-theme (load-theme lye-default-theme t)))))
