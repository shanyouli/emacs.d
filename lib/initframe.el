;;; lib/initframe.el -*- lexical-binding: t; -*-

;;;###autoload
(defun initframe-get-display-info (&optional name)
  "获取 emacs 所在显示器大小，当 `NAME' 为 width 表示获取 width，
为 `height' 获取 height。 默认获取 width 和 height"
  (let ((infos (assq 'geometry (frame-monitor-attributes))))
    (cond ((eq name 'width) (nth 3 infos))
          ((eq name 'height) (nth 4 infos))
          (t (last infos 2)))))

;;;###autoload
(defun initframe-init-default-frame (&optional frame wratio hratio)
  "根据比例设置窗口 size，`WRATIO' 表示窗口宽度占显示器的比例，
`HRATIO' 表示窗口高度占显示器的高度比例。
`FRAME'是否指定窗口，默认为当期聚焦窗口."
  (interactive)
  (let* ((display-size (initframe-get-display-info))
         (x-width (if wratio
                       (truncate (- (* (car display-size) wratio) 20))
                     (or (alist-get 'width default-frame-alist)
                         (truncate (- (* (car display-size) 0.5) 20)))))
         (y-height (if hratio
                       (truncate (- (* (cadr display-size) hratio)))
                     (or (alist-get 'height default-frame-alist)
                         (truncate (- (* (cadr display-size) 0.5)))))))
    (set-frame-size (or frame (selected-frame)) x-width y-height (>= width 200))))


;; EmacsClient 配置
(defvar initframe-client-frame nil "Store the generated EmacsClient frame")
(defconst initframe-client-frame-prefix-name "EmacsClient" "指定emacs client 名称。")
(defconst initframe-client-param 'my-client "指定属性名称")


(defvar initframe-client-frame-parameters
  `((name . ,initframe-client-frame-prefix-name)
    (width . 88)
    (height . 27)
    (transient . t)
    ,@(when IS-MAC `((window-system . ns)
                     (menu-bar-lines . 1)))
    ,@(when IS-LINUX
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "WAYLAND_DISPLAY")
                          (getenv "DISPLAY")
                          ":0")))))
  "emacs client 默认 frame 参数")

(defun initframe-make-client-frame (&optional params)
  "构建一个有 my-client 属性的 frame。`Params' frame 的额外属性。"
  (make-frame (append `((,initframe-client-param . t)) params)))

(defun initframe-frame-is-client-frame-p (&optional frame)
  "Return t 如果当前 frame 就是 Emacs-Client"
  (string-prefix-p initframe-client-frame-prefix-name
                   (frame-parameter (or frame (selected-frame)) 'name)))
(defun initframe-client-frame-is-live-p ()
  "判断是否存在以 EmacsClient 开头的 frame 存活。"
  (if (and initframe-client-frame (frame-live-p initframe-client-frame))
    initframe-client-frame
    (setq initframe-client-frame (cl-find-if (lambda (f) (initframe-frame-is-client-frame-p f))
                                             (frame-list)))
    initframe-client-frame))


;;;###autoload
(defun initframe-open-client-frame (&optional $file)
  "打开或聚焦到 client-frame。如果存在路径，则打开该文件。"
  (let ((frame (or (initframe-client-frame-is-live-p)
                   (initframe-make-client-frame initframe-client-frame-parameters))))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (with-selected-frame frame (set-frame-parameter frame 'name nil))
          (when (and $file (file-exist-p $file))
            (with-selected-frame frame (find-file $file)))
          t)
      nil)))

(provide 'initframe)
