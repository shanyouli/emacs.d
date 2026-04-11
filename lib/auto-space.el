;;; lib/auto-space.el -*- lexical-binding: t; -*-

;; 可以使用零宽字符, "\u200B"
(defvar my-space-char " " "默认空格格式，")

(defun my-add-space-between-chinese-and-english ()
  "在中英文之间自动添加空格。"
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point)))))
    (when (and current-char prev-char
               (or (and (my-is-chinese-character prev-char) (my-is-halfwidth-character current-char))
                   (and (my-is-halfwidth-character prev-char) (my-is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; 检查前一个字符不是空格
      (save-excursion
        (goto-char (1- (point)))
        (insert my-space-char)))))

(defun my-is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun my-is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                )))

(defun my-delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'my-add-space-between-chinese-and-english))

;; 定义局部的 minor mode
;;;###autoload
(define-minor-mode my-auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global nil
  (if my-auto-space-mode
      (add-hook 'post-self-insert-hook 'my-add-space-between-chinese-and-english nil t)
    (remove-hook 'post-self-insert-hook 'my-add-space-between-chinese-and-english t)))

;; 定义全局的 minor mode
;;;autoload
(define-globalized-minor-mode global-my-auto-space-mode
  my-auto-space-mode ; 参数 A: 对应的局部模式变量名
  (lambda () (my-auto-space-mode +1)) ;启动局部模式时调用的函数
  )
