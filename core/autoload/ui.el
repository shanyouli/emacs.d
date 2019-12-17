;;; core/autoload/ui.el -*- lexical-binding: t -*-

;;;###autoload
(defun lye-font-initialize ()
  "Font initalization."
  (when (display-graphic-p)
    (require 'lib-font)
    (setq lib-font-english (or lye-en-font
                               (let ((font "Fantasque Sans Mono"))
                                 (if (lib-font-exist-p font)
                                     font))
                               (let ((font "Fira Code"))
                                 (if (lib-font-exist-p font)
                                     font)))
          lib-font-chinese (or lye-zh-font
                               (and IS-LINUX "WenQuanYi Micro Hei")
                               (and IS-MAC "Hiragio Sans GB")
                               (and IS-WINDOWS "Microsoft Yahei"))
          lib-font-default-size (or lye-default-font-size 14))
    (lib-font-initialize-monospace)))

;;;###autoload
(defun lye|font-initialize (frame)
  (with-selected-frame frame (lye-font-initialize)))

;;;###autoload
(defun lye|frame-default-size-with-frame (frame)
  (lib-frame-default-size frame))
