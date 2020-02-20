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
(defcustom lye-en-font (cl-loop for font in '("Fantasque Sans Mono"
                                              "FantasqueSansMono NF"
                                              "Hasklig"
                                              "Fira Code"
                                              "Source Code Pro")
                                when (lye-font-installed-p font)
                                return font)
  "Customize English font." :type 'string)
(defcustom lye-zh-font (cl-loop for font in '("Adobe Heiti Std"
                                              "WenQuanYi Micro Hei Mono"
                                              "Sarasa Mono SC"
                                              "Microsoft Yahei")
                                when (lye-font-installed-p font)
                                return font)
  "Customize Chinese font." :type 'string)
(defcustom lye-unicode-font (cl-loop for font in
                                     '("DejaVu Sans Mono"
                                       "Symbola"
                                       "Apple Symbols"
                                       "Symbol"
                                       "icons-in-terminal")
                                     when (lye-font-installed-p font)
                                     return font)
  "Customize unicode font."
  :type 'string)
(defcustom lye-default-font-size nil "Customize font size." :type 'integer)

;; Theme variables
(defcustom lye-theme-default  nil "Lye Default themes." :type '(symbol list))
(defcustom lye-theme-use-list nil "Custom Switches list." :type 'list)
(defcustom lye-theme-set-Lat-and-lon nil
  "With sunrise and sunset to set different themes.

 Format: Use latitude and longitude: (30.93 . 113.92)
         Use custom time:            (\"08:30\" . \"18:30\")"
  :type 'list)

;; Title
(when (display-graphic-p)
  (setq frame-title-format
        '("Lye Emacs - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   (buffer-name)))))
  (setq icon-title-format frame-title-format))

;; Not scroll-bar, tool-bar and menu-bar-mode
(run-with-idle-timer! :defer 3
  (setq tool-bar-mode nil scroll-bar-mode nil)
  (unless IS-MAC
    (setq menu-bar-mode nil)))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      initial-buffer-choice nil
      ;;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
      frame-resize-pixelwise t)


;;
;;; Frame size
;; FIX: see https://github.com/syl20bnr/spacemacs/issues/4365#issuecomment-202812771
;; 获得整 个frame 大致的行数，和列数
(defun lye-get-columns-in-the-entire-frame ()
  (if (display-graphic-p)
      (let ((edges (frame-edges)))
        (truncate (/ (- (nth 2 edges) (nth 0 edges)) (default-font-width))))))

(defun lye-get-lines-in-the-entire-frame ()
  (if (display-graphic-p)
      (let ((edges (frame-edges)))
        (truncate (/ (- (nth 3 edges) (nth 1 edges)) (default-font-height))))))
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

(push (lib-f-join lye-etc-dir "doom-themes/themes/") custom-theme-load-path)

(defcustom lye-autoload-switch-dark-or-light-p nil
  "If it is non-nil, Not use `lib-theme-switch-theme'."
  :type 'boolean)
(defcustom lye-autoload-switch-theme-and-time nil
  "Default dark or light theme"
  :type 'list)
(defcustom lye-theme-list nil "Using theme list." :type 'list)
(defcustom lye-default-theme nil "Lye Default theme." :type 'list)

(defvar lye--current-theme nil)
(setq lye-autoload-switch-theme-and-time
      '((doom-one  doom-molokai) . (30.93 . 113.92)))

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
          lye-init--font (face-attribute 'default :font))
    (set-face-attribute 'default nil :height 100))))

(defun lye-init-fonts-with-daemon-h (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (unless lye-init--font (lye-init-fonts-h))
    (unless (alist-get 'width default-frame-alist)
      ;; (lye-init-default-size)
      )))

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
;;; Initialize UI
(defvar lye-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defun lye-init-ui-h ()
  "Initialize Lye's user interface by applying all its advice and hooks."
  (run-hook-wrapped 'lye-init-ui-hook #'lye-try-run-hook)
  (lye-init-frame-size))

(add-hook! 'lye-load-theme-hook
  (lye-init-extra-fonts-h)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config))
(add-hook! 'window-setup-hook #'lye-init-ui-h)
(add-hook (if (daemonp) 'after-make-frame-functions 'lye-init-ui-hook)
          #'lye|initialize-theme)
(add-hook (if (daemonp) 'after-make-frame-functions 'lye-init-ui-hook)
          #'lye-init-fonts-with-daemon-h)
;; (when (daemonp)
;;   (add-hook! 'after-make-frame-functions (unless lye-init--font (lye-init-fonts-h))))

;;
;;; Line-Number
;; 文件超过10000行，不显示行号，只留4位吧
(setq display-line-numbers-width-start 4)
(defun lye-display-line-numbers ()
  "当文件的列宽 <86 或者文件的 Major－mode 为 org－mode 且行数大于 1000,
不显示行号。"
  (if (and (> (lye-get-columns-in-the-entire-frame) 86)
           (or (not (eq major-mode 'org-mode))
               (< (line-number-at-pos (point-max) 1000))))
      (display-line-numbers-mode +1)
    (display-line-numbers-mode -1)))

(add-hook! '(prog-mode-hook org-mode-hook conf-mode-hook nxml-mode-hook)
  #'lye-display-line-numbers)
