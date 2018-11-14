;;; init-scheme --- Scheme COnfigure
;;; Commentary:

;;; @see http://www.yinwang.org/blog-cn/2013/04/11/scheme-setup
;;; Code:
(require 'cmuscheme)
(setq scheme-program-name "scheme")

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable 'scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((=1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*")
         (mapcar (lambda (w) (buffer-name (window-buffer w)))
                 (window-list))
         :test 'equal))
   (other-window 1)
   (switch-tobuffer "*scheme*")
   (other-window -1)))
(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))
(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

;;; parenface.el
;;; parenface.el --- Provide a face for parens in lisp modes.
;; Based on parenface.el from Dave Pearson <davep@davep.org>

(defvar paren-face 'paren-face)

(defface paren-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
     (let* ((regexp "(\\|)\\|\\[\\|\\]\\|{\\|}")
            (match (assoc regexp ,keywords)))
       (unless (eq (cdr match) paren-face)
         (setq ,keywords (cons (cons regexp paren-face) ,keywords))))))

(add-hook 'yin-mode-hook              (paren-face-add-support yin-font-lock-keywords))
(add-hook 'scheme-mode-hook           (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook             (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook       (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))


(provide 'init-scheme)
;;; init-scheme.el ends here
