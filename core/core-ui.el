;;; core/core-ui.el.el -*- lexical-binding: t -*-

;; (setq facy-splash-image logo) ; Logo

;;
;;; Variables

;; Frame variables
(defcustom lye-frame-height-scale nil
  "The height of window screen ratio accounted for emacs-frame."
  :type 'float)

(defcustom lye-frame-width-scale nil
  "The Wedth of window screen ratio accounted for emacs-frame.")

(defcustom lye-frame-use-fullframe (not IS-WINDOWS)
  "是否启动全屏。" :type 'boolean)

;; Font variables
(defcustom lye-en-font nil "Customize English font." :type 'string)
(defcustom lye-zh-font (cl-loop for font in '("Adobe Heiti Std"
                                              "WenQuanYi Micro Hei Mono"
                                              "Microsoft Yahei"
                                              "Sarasa Mono SC")
                                when (lye-font-installed-p font)
                                return font)
  "Customize Chinese font." :type 'string)
(defcustom lye-unicode-font (cl-loop for font in
                                     '("Symbola"
                                       "Apple Symbols"
                                       "Symbol"
                                       "icons-in-terminal")
                                     when (lye-font-installed-p font)
                                     return font)
  "Customize unicode font."
  :type 'string)
(defcustom lye-default-font-size nil "Customize font size." :type 'integer)

;; Theme variables
(defcustom lye-default-theme  nil
  "Lye Default themes. The format is:
THEME, or
`((LIGHT-THEME DARK-THEME) (\"08:30\" \"18:30\"))'
or
`((LIGHT-THEME DARK-THEME) (LAT LONG))'
"
  :type '(symbol list))

(defcustom lye-theme-use-list nil "Custom Switches list." :type 'list)

;;
;;; Frame size
;; FIX: see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
(defun fullscreen-toggle ()
  "Toggle fullscreen status."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun lye-init-default-size (&optional frame)
  (interactive)
  (let ((x-width (or (alist-get 'width default-frame-alist)
                     (truncate (/ (- (* (x-display-pixel-width)
                                        (or lye-frame-width-scale 0.5)) 24)
                                  (frame-char-width)))))
        (y-height (or (alist-get 'height default-frame-alist)
                      (truncate (/ (* (x-display-pixel-height)
                                      (or lye-frame-height-scale 0.618))
                                   (frame-char-height))))))
    (set-frame-size (or frame (selected-frame)) x-width y-height)))

(defun lye-init-frame-size ()
  "When `lye-frame-use-fullframe' is t, use fullframe.
When `lye-frame-use-fullfrmae' is nil, use default-frame."
  (if lye-frame-use-fullframe
      (if (not IS-MAC)
          (set-frame-parameter nil 'fullscreen 'fullboth)
        ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
        ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
        ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
        ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
        ;;
        ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
        ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
        ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
        ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
        (setq ns-use-native-fullscreen nil)
        (setq ns-use-fullscreen-animation nil)

        ;; 默认先最大化。
        (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

        (run-at-time "2sec" nil (lambda () (toggle-frame-fullscreen))))
    (when (display-graphic-p)
      (let* ((x-max (x-display-pixel-width))
             (y-max (x-display-pixel-height))
             (x-scale (or lye-frame-width-scale 0.5))
             (y-scale (or lye-frame-height-scale 0.618))
             (x-point (truncate (* x-max (/ (- 1 x-scale) 2))))
             (y-point (truncate (* y-max (/ (- 1 y-scale) 2))))
             (x-width (truncate (- (* x-max x-scale) 24)))
             (y-height (truncate (* y-max y-scale)))
             (frame (selected-frame)))
        (set-frame-position frame x-point y-point)
        (set-frame-size frame x-width y-height t))))
  (if (display-graphic-p)
      (run-at-time
       "3sec" nil
       (lambda ()
         (setf (alist-get 'width default-frame-alist)
               (truncate (/ (- (* (x-display-pixel-width)
                                  (or lye-frame-width-scale 0.5)) 24)
                            (frame-char-width))))
         (setf (alist-get 'height default-frame-alist)
               (truncate (/ (* (x-display-pixel-height)
                               (or lye-frame-height-scale 0.618))
                            (frame-char-height))))))))

;;
;;; THEME
(push (lib-f-join lye-etc-dir "doom-themes/themes/") custom-theme-load-path)

(defvar lye--current-theme nil)

(defvar lye-load-theme-hook nil)
(defun lye--run-load-theme-hooks-a (theme &optional _no-confirm no-enable)
  "Set up `lye-load-theme-hook' to run after `load-theme' is called."
  (unless no-enable
    (setq lye--current-theme theme)
    (run-hooks 'lye-load-theme-hook)))
(advice-add #'load-theme :after-while #'lye--run-load-theme-hooks-a)

;; 每次执行 `load-theme' 之前自动执行 disabled-theme.
(defun lye--run-load-theme-before-a (&rest _)
  (when custom-enabled-themes
    (mapc #'disable-theme custom-enabled-themes)))
(advice-add #'load-theme :before #'lye--run-load-theme-before-a)

(defun core-ui::time-number-to-string (timer-number)
  "Conver TIME-NUMBER hours to HH:MM. eg: Cover 16.8 hours to 16:48"
  (let* ((time-integer (truncate timer-number))
         (time-decimal (truncate (* 60 (- timer-number time-integer)))))
    (concat (and (< time-integer 10) "0")
            (number-to-string time-integer)
            ":"
            (and (< time-decimal 10) "0")
            (number-to-string time-decimal))))

(defun core-ui::get-sunrise-sunset-time (lat lon)
  "Returns the sunrise and sunset times."
  (require 'solar)
  (let* ((calendar-latitude lat)
         (calendar-longitude lon)
         (result (solar-sunrise-sunset (calendar-current-date))))
    (cons (core-ui::time-number-to-string (caar result))
          (cons (core-ui::time-number-to-string (caadr result))
                nil))))
(defun lib-theme--string-time-to-list (time-list)
  "Convert HH:MM:SS to (HH MM SS).")
(defun core-ui::time-to-second (time-string)
  (let ((time-list (mapcar #'string-to-number (split-string time-string ":"))))
    (+ (or (nth 2 time-list) 0)
       (* 60 (+ (* 60 (nth 0 time-list))
                (nth 1 time-list))))))

(defun core-ui::switch-themes ()
  "Change the light and dark themes over time."
  (let ((themes (nth 0 lye-default-theme))
        (times (let ((time-or-lat (nth 1 lye-default-theme)))
                 (if (stringp (car time-or-lat))
                     time-or-lat
                   (core-ui::get-sunrise-sunset-time (nth 0 time-or-lat)
                                                     (nth 1 time-or-lat)))))
        (ctime (substring (current-time-string) 11 19)))
    (if (or (not lye--current-theme) (memq lye--current-theme themes))
        (let ((sunrise (nth 0 times))
              (sunset (nth 1 times))
              next-theme
              next-time)
          (cond ((string> sunrise ctime) (setq next-theme (nth 1 themes)
                                               next-time sunrise))
                ((string> sunset ctime) (setq next-theme (nth 0 themes)
                                              next-time sunset))
                (t (setq next-theme (nth 1 themes)
                         next-time (+ (core-ui::time-to-second sunrise)
                                      (- 86400
                                         (core-ui::time-to-second ctime))))))
          (load-theme next-theme t)
          ;; (cancel-function-timers #'core-ui::switch-themes)
          (run-at-time next-time nil #'core-ui::switch-themes))
      (cancel-function-timers #'core-ui::switch-themes))))

(defun core-ui::initialize-theme-h  (&optional frame)
  "Change themes."
  (when (display-graphic-p)
    (with-selected-frame (or frame (selected-frame))
      (unless (null lye-default-theme)
        (pcase lye-default-theme
          ((pred listp) (core-ui::switch-themes))
          ((pred symbolp) (load-theme lye-default-theme t)))))))

(setq lye-theme-use-list
      (cl-remove-if-not (lambda (x) (string-prefix-p "doom-" (symbol-name x)))
                        (custom-available-themes)))
(setq lye-default-theme
      '((doom-one doom-molokai) (30.91  113.92)))
;;
;;; font
(defvar lye-init--font nil "记录 emacs 初始化使用的字体格式。")
(defun lye-init-fonts-h ()
  "Loads `lye-en-font'."
  (cond
   (lye-en-font
    (setq lye-init--font (font-spec :family lye-en-font
                                    :size (or lye-default-font-size 14)))
    (cl-pushnew
     ;; Avoiding `set-frame-font' because it does a lot of extra, expensive
     ;; work we can avoid by setting the font frame parameter instead.
     (cons 'font (font-xlfd-name lye-init--font))
     initial-frame-alist :key #'car :test #'eq)
    (cl-pushnew
     ;; Avoiding `set-frame-font' because it does a lot of extra, expensive
     ;; work we can avoid by setting the font frame parameter instead.
     (cons 'font (font-xlfd-name lye-init--font))
     default-frame-alist :key #'car :test #'eq))
   ((display-graphic-p)
    ;; We try our best to record your system font,
    ;; can still use it to compute a larger font size with.
    (setq font-use-system-font t
          lye-init--font (face-attribute 'default :font)))))

(defun lye-init-fonts-with-daemon-h (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (unless lye-init--font (lye-init-fonts-h))))

(defun lye-init-extra-fonts-h (&optional frame)
  "Loads `lye-unicode-font' `lye-zh-font'."
  (condition-case e
      (with-selected-frame (or frame (selected-frame))
        (when (and lye-zh-font (fboundp 'set-fontset-font))
          (set-fontset-font t '(#x4e00 . #x9fff)
                            (font-spec :family lye-zh-font :size 14)))
        (when (and lye-unicode-font (fboundp 'set-fontset-font))
          (set-fontset-font t 'unicode
                            (font-spec :family lye-unicode-font)
                            nil 'prepend)))
    ((debug error)
     (if (string-prefix-p "Font not available: " (error-message-string e))
         (lwarn 'lye-core-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr e) :family))
       (signal 'lye-error e)))))

(add-hook! 'after-init-hook 'lye-init-fonts-h)

;;
;;; Line-Number
;; 文件超过10000行，不显示行号，只留4位吧
(setq display-line-numbers-width-start 4)
(defun lye-display-line-numbers ()
  "当文件的列宽 <86 或者文件的 Major－mode 为 org－mode 且行数大于 1000,
不显示行号。"
  (if (and (or (not (display-graphic-p))
               (> (window-width) 86))
           (or (not (eq major-mode 'org-mode))
               (< (line-number-at-pos (point-max) 1000))))
      (display-line-numbers-mode +1)
    (display-line-numbers-mode -1)))

(add-hook! '(prog-mode-hook org-mode-hook conf-mode-hook nxml-mode-hook)
  #'lye-display-line-numbers)

;; Title
(when (display-graphic-p)
  (setq frame-title-format
        '("Lye Emacs - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   (buffer-name)))))
  (setq icon-title-format frame-title-format))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      initial-buffer-choice nil
      ;;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
      frame-resize-pixelwise t)

;;
;;; Initialize UI
(defvar lye-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defun lye-init-ui-h ()
  "Initialize Lye's user interface by applying all its advice and hooks."
  ;; Not scroll-bar, tool-bar and menu-bar-mode
  (setq tool-bar-mode nil scroll-bar-mode nil)
  (unless IS-MAC (setq menu-bar-mode nil))

  (run-hook-wrapped 'lye-init-ui-hook #'lye-try-run-hook)
  (lye-init-frame-size))

(add-hook! 'lye-load-theme-hook
  (lye-init-extra-fonts-h)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config))
(add-hook! 'window-setup-hook #'lye-init-ui-h)
(add-hook (if (daemonp) 'after-make-frame-functions 'lye-init-ui-hook)
          #'core-ui::initialize-theme-h)
(add-hook (if (daemonp) 'after-make-frame-functions 'lye-init-ui-hook)
          #'lye-init-fonts-with-daemon-h)
