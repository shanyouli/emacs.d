;;; core/autoload/font.el -*- lexical-binding: t -*-

;;;###autoload
(defun lye/font-initialize+ (&optional english chinese default-size)
  "Font intialization"
  (when (display-graphic-p)
    (setq mdf-english (cond ((and english (mdf/font-exist-p! english)) english)
                            ((mdf/font-exist-p! "Fantasque Sans Mono")
                             "Fantasque Sans Mono")
                            ((mdf/font-exist-p! "Fira Code") "Fira Code"))
          mdf-cjk (cond ((and chinese (mdf/font-exist-p! chinese)) chinese)
                        (system/linux "WenQuanYi Micro Hei")
                        (system/mac "Hiragio Sans GB")
                        (system/windows "Microsoft Yahei")))
    (if default-size
        (if (listp default-size)
            (setq mdf-size-pair default-size)
          (setq mdf-default-size default-size)))
    (mdf/monospace-font-initialize+)))

;;;###autoload
(defun lye/font-initialize-frame+ (frame &optional english chinese default-size)
  (with-selected-frame frame
    (lye/font-initialize+)))