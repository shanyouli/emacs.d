;;; bundles/elisp/config.el -*- lexical-binding: t -*-

(add-hook! 'emacs-lisp-mode-hook
  ;;Beautify emacs-lisp
  (push '("lambda" .  "λ") prettify-symbols-alist)
  (prettify-symbols-mode +1)

  ;; Function that highlights global variables
  (elispfl-mode +1)
  ;; Identation
  (sly-el-indent-setup))

(with-eval-after-load  'ielm
  (elispfl-ielm-mode))


(with-eval-after-load 'elisp-mode
  (when (boundp 'elisp-flymake-byte-compile-load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

    ;; Add remove buttons for advices
  (add-hook 'help-mode-hook 'cursor-sensor-mode)


  (defun function-advices (function)
    "Return FUNCTION's advices."
    (let ((function-def (advice--symbol-function function))
          (ad-functions '()))
      (while (advice--p function-def)
        (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
        (setq function-def (advice--cdr function-def)))
      ad-functions))

  (defun add-button-to-remove-advice (buffer-name function)
    "Add a button to remove advice."
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (save-excursion
          (goto-char (point-min))
          (let ((ad-index 0)
                (ad-list (reverse (function-advices function))))
            (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']\\.?$" nil t)
              (let* ((name (string-trim (match-string 1) "'" "'"))
                     (advice (or (intern-soft name) (nth ad-index ad-list))))
                (when (and advice (functionp advice))
                  (let ((inhibit-read-only t))
                    (insert "\t")
                    (insert-text-button
                     "[Remove]"
                     'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                     'help-echo (format "%s" advice)
                     'action
                     ;; In case lexical-binding is off
                     `(lambda (_)
                        (when (yes-or-no-p (format "Remove %s ? " ',advice))
                          (message "Removing %s of advice from %s" ',function ',advice)
                          (advice-remove ',function ',advice)
                          (if (eq major-mode 'helpful-mode)
                              (helpful-update)
                            (revert-buffer nil t))))
                     'follow-link t))))
              (setq ad-index (1+ ad-index))))))))

  (define-advice describe-function-1 (:after (function) advice-remove-button)
    (add-button-to-remove-advice "*Help*" function))

  ;; Remove hook
  (defun remove-hook-at-point ()
    "Remove the hook at the point in the *Help* buffer."
    (interactive)
    (unless (or (eq major-mode 'help-mode)
                (eq major-mode 'helpful-mode)
                (string= (buffer-name) "*Help*"))
      (error "Only for help-mode or helpful-mode"))
    (let ((orig-point (point)))
      (save-excursion
        (when-let
            ((hook (progn (goto-char (point-min)) (symbol-at-point)))
             (func (when (and
                          (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                              (goto-char orig-point))
                          (sexp-at-point))
                     (end-of-sexp)
                     (backward-char 1)
                     (catch 'break
                       (while t
                         (condition-case _err
                             (backward-sexp)
                           (scan-error (throw 'break nil)))
                         (let ((bounds (bounds-of-thing-at-point 'sexp)))
                           (when (<= (car bounds) orig-point (cdr bounds))
                             (throw 'break (sexp-at-point)))))))))
          (when (yes-or-no-p (format "Remove %s from %s? " func hook))
            (remove-hook hook func)
            (if (eq major-mode 'helpful-mode)
                (helpful-update)
              (revert-buffer nil t))))))))

;; Macro expander face
(face-spec-set 'macrostep-expansion-highlight-face
               `((t (:background ,(face-background 'tooltip) :extend t))))

(add-hook! 'after-load-theme-hook
    (set-face-background 'macrostep-expansion-highlight-face
     (face-background 'tooltip)))

;; helpful
(add-hook! 'helpful-mode-hook (cursor-sensor-mode +1))

(with-eval-after-load 'counsel
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(with-eval-after-load 'apropos
  ;; patch apropos buttons to call helpful instead of help
  (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
    (button-type-put
     fun-bt 'action
     (lambda (button)
       (helpful-callable (button-get button 'apropos-symbol)))))
  (dolist (var-bt '(apropos-variable apropos-user-option))
    (button-type-put
     var-bt 'action
     (lambda (button)
       (helpful-variable (button-get button 'apropos-symbol))))))

  ;; Add remove buttons for advices
(define-advice helpful-callable (:after (function) advice-remove-button)
  (add-button-to-remove-advice (helpful--buffer function t) function))

(with-eval-after-load 'helpful
  (with-no-warnings
    (defun my-helpful--navigate (button)
      "Navigate to the path this BUTTON represents."
      (find-file-other-window (substring-no-properties (button-get button 'path)))
      ;; We use `get-text-property' to work around an Emacs 25 bug:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
      (-when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
        (helpful--goto-char-widen pos)))
    (advice-add #'helpful--navigate :override #'my-helpful--navigate)))

;; elisp-demos
(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
(with-eval-after-load 'helpful
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
