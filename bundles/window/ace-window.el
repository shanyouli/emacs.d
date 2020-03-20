;;; bundles/window/ace-window.el -*- lexical-binding: t -*-

(face-spec-set 'aw-mode-line-face
               '((t (:inherit mode-line-emphasis :bold t :underline t))))
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit font-lock-keyword-face :bold t :height 2.0 :box t)))))
(add-hook! 'emacs-startup-hook (ace-window-display-mode +1))
(with-eval-after-load 'ace-window
  ;; Select widnow via `M-1'...`M-9'
  (defun aw--get-window-number (&optional win)
    "If win is nil, Get current window number. other Get `Win' window number!"
    (string-to-number (window-parameter (or win (selected-window)) 'ace-window-path)))
  (defun aw--select-window-by-number (&optional arg)
    (interactive "P")
    (let* ((n (cond
                ((integerp arg) arg)
                ((eq arg '-) 0) ; the negative argument
                (arg (aw--get-window-number))
                ((called-interactively-p 'any)
                 (let ((user-input-str (read-from-minibuffer "Window number: ")))
                   (if (not (string-match-p "[+-]?[0-9]+\.*" user-input-str))
                       (aw--get-window-number)
                     (string-to-number user-input-str))))
                (t (aw--get-window-number))))
           (w (car (seq-filter
                    (lambda (win)
                      (and (window-live-p win)
                           (eq n (aw--get-window-number win))))
                    (aw-window-list)))))
      (if w
          (aw-switch-to-window w)
        (error "No window numberd %d" n))))
  (defun aw-select-window-0 ()
    "Jump window last number."
    (interactive)
    (aw-switch-to-window (car (last (aw-window-list)))))
  (defun aw-select-window-1 ()
    "Jump to window 1."
    (interactive)
    (aw--select-window-by-number 1))
  (defun aw-select-window-2 ()
    "Jump to window 2."
    (interactive)
    (aw--select-window-by-number 2))
  (defun aw-select-window-3 ()
    "Jump to window 3."
    (interactive)
    (aw--select-window-by-number 3))
  (defun aw-select-window-4 ()
    "Jump to window 4."
    (interactive)
    (aw--select-window-by-number 4))
  (defun aw-select-window-5 ()
    "Jump to window 5."
    (interactive)
    (aw--select-window-by-number 5))
  (defun aw-select-window-6 ()
    "Jump to window 6."
    (interactive)
    (aw--select-window-by-number 6))
  (defun aw-select-window-7 ()
    "Jump to window 7."
    (interactive)
    (aw--select-window-by-number 7))
  (defun aw-select-window-8 ()
    "Jump to window 8."
    (interactive)
    (aw--select-window-by-number 8))
  (defun aw-select-window-9 ()
    "Jump to window 9."
    (interactive)
    (aw--select-window-by-number 9))

  (with-eval-after-load 'super-save
    (advice-add 'aw--select-window :before #'super-save-command-advice)
    (advice-add 'ace-window :before #'super-save-command-advice)
    (push 'aw--select-window super-save-triggers)
    (push 'ace-window super-save-triggers)))
