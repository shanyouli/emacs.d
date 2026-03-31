;;; lib/scratch.el -*- lexical-binding: t; -*-
(declare-function cl-loop "cl-macs")

(defvar my-scratch-root-dir
  (expand-file-name "scratch-labs/"
                    (or (bound-and-true-p doom-local-dir) user-emacs-directory))
  "my test 语言 scratch 的根目录")

(defvar my-scratch-language-configs
  '((python
     :init ("uv" "init" "--lib" "-q")
     :extension "py"
     :entry "main.py"
     :shebang "#!/usr/bin/env python")
    (javascript :init ("bun" "init" "-y") :extension "js" :entry "index.js" :pre t)
    (typescript :init ("bun" "init" "-y") :extension "ts" :entry "index.ts")
    (bash :init nil :extension "sh" :shebang "#!/usr/bin/env bash"))
  "配置列表: 语言 (:init 初始化命令列表 :extension 后缀 :entry 入口文件)")

(defun my-scratch-exec-first-available (commands)
  "查找并执行第一个可用的初始化工具。"
  (let ((cmds
         (if (stringp (car commands))
             (list commands)
           commands))
        (success nil)
        (temp-buffer (get-buffer-create "*scratch-init-error*"))
        )
    (cl-loop for cmd in cmds
             when (executable-find (car cmd))
             do
             (progn
               (apply #'call-process (car cmd) nil temp-buffer nil (cdr cmd))
               (setq success t))
             until success)
    success))

(defun my-scratch-create-new-entry (path &optional shebang)
  "创建基础入口文件，并根据配置插入自定义 Shebang。"
  (with-temp-file path
    (if shebang
        (insert shebang "\n\n")))
  (when shebang
    (set-file-modes path #o755)))

;;;###autoload
(defun my-open-scratch-workspace (lang-input)
  "支持自定义 shebang 的 scratch"
  (interactive (list
                (completing-read
                 "选择语言(输入任意字符进入 zztemp 目录:)"
                 (mapcar #'car my-scratch-language-configs))))
  (unless (file-exists-p my-scratch-root-dir)
    (make-directory my-scratch-root-dir t))
  (let* ((lang-sym (intern lang-input))
         (config (cdr (assoc lang-sym my-scratch-language-configs)))
         (is-zztemp (not config)))
    (if is-zztemp
        ;; --- ZZTEMP 流程，无 Git 和归档控制
        (let ((lang-root (expand-file-name "zztemp" my-scratch-root-dir))
              (entry-name (concat "test." (read-string "输入后缀名(txt,log): "))))
          (unless (file-exists-p lang-root)
            (make-directory lang-root t))
          (find-file (expand-file-name entry-name lang-root))
          (message (format "open %s" entry-name)))
      (let* ((entry-name
              (or (plist-get config :entry)
                  (concat
                   "test." (or (plist-get config :extension) lang-input))))
             (lang-root (expand-file-name lang-input my-scratch-root-dir))
             (init-commands (plist-get config :init))
             (pre-is-ok (plist-get config :pre))
             (entry-path (expand-file-name entry-name lang-root))
             (today (format-time-string "%Y%m%d"))
             (default-directory lang-root))
        (if (file-exists-p lang-root)
            (when (file-exists-p entry-path)
              (let* ((mtime (nth 5 (file-attributes entry-path)))
                     (file-date (format-time-string "%Y%m%d" mtime))
                     (entry-path (expand-file-name entry-name lang-root))
                     (archive-path (expand-file-name "archive" lang-root)))
                (unless (string= file-date today)
                  (unless (file-exists-p archive-path)
                    (make-directory archive-path))
                  (rename-file
                   entry-path
                   (expand-file-name (format "%s-%s" file-date entry-name)
                                     archive-path))
                  (shell-command
                   (format "git add %s"
                           (shell-quote-argument
                            (format "%s/%s-%s"
                                    archive-path file-date entry-name)))
                   nil)
                  (shell-command
                   (format "git commit -m 'Archive %s'" file-date) nil)
                  (my-scratch-create-new-entry entry-path
                                               (plist-get config :shebang)))))
          (make-directory lang-root t)
          (let ((default-directory lang-root))
            ;; 使用专业工具对项目进行初始化
            (when pre-is-ok
              (my-scratch-create-new-entry
               (expand-file-name entry-name lang-root)
               (plist-get config :shebang)))
            (when init-commands
              (my-scratch-exec-first-available init-commands))
            (unless (file-exists-p (expand-file-name ".git" lang-root))
              (shell-command "git init" nil)
              (with-temp-file ".gitignore"
                (insert "__pycache__/\nnode_modules/\n.venv/\n")))
            (unless (file-exists-p entry-path)
              (my-scratch-create-new-entry entry-path
                                           (plist-get config :shebang)))
            ;; 进行第一次 git commit
            (shell-command "git add ." nil)
            (shell-command (format "git commit -m 'First commit on %s'" today) nil)))
        (find-file entry-path)
        (message "scratch by %s" lang-input)))))

;;;###autoload
(defun my-scratch-set-language-config (lang-sym &rest props)
  "动态添加或修改语言配置。
用法示例:
(my-scratch-set-language-config 'rust :init '(\"cargo\" \"init\") :extension \"rs\" :entry \"src/main.rs\")"
  (let ((exists (assoc lang-sym my-scratch-language-configs)))
    (if exists
        (setcdr exists props)
      (push (cons lang-sym props) my-scratch-language-configs)))
  ;; (message "配置已更新: %s" lang-sym)
  )
