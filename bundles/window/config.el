;;; bundles/window/config.el -*- lexical-binding: t -*-

;; Directional window-selection routines
(add-hook! 'after-init-hook 'windmove-default-keybindings)

;; Restore old window configurations
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"))
(add-hook! 'after-init-hook 'winner-mode)

;; Quickly switch windows
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
               (if (= (car this-win-edges)
                      (car (window-edges (next-window))))
                   'split-window-horizontally
                 'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(pcase lye-use-switch-windows-package
  ('ace-window (lib-load-relative "ace-window" t t))
  ('winum (add-hook! 'emacs-startup-hook (winum-mode +1))))



;; Enforce rules for popups
;; @see https://github.com/seagle0128/.emacs.d/blob/f8a53fbc60414c98142e0fb9d33c8e6353db9347/lisp/md-window.el#L127
(add-hook! 'after-init-hook (shackle-mode +1))

;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

(with-eval-after-load 'shackle
  (defun shackle-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

  ;; Add keyword: `autoclose'
  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

      ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
    (with-eval-after-load 'org-macs
      (advice-add #'org-switch-to-buffer-other-window
                  :override #'switch-to-buffer-other-window))

    ;; Rules
      (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          (("*compilation*" "*Compile-Log*") :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :align 'below)
          ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :size 0.4 :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 15 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ((" *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.4 :align 'below :autoclose t)
          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
          (" *Install vterm" :size 0.35 :same t :align 'below)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)

          ("*ert*" :size 15 :align 'below :autoclose t)
          (overseer-buffer-mode :size 15 :align 'below :autoclose t)

          (" *Flycheck checkers*" :select t :size 0.4 :align 'below :autoclose t)
          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
           :select t :size 0.3 :align 'below :autoclose t)

          (profiler-report-mode :select t :size 0.5 :align 'below)
          ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)

          ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align 'below)
          ("*prolog*" :size 0.4 :align 'below)

          ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (tabulated-list-mode :size 0.4 :align 'below))))
