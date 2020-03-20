;;; bundles/evil/config.el.el -*- lexical-binding: t -*-

(add-hook! 'after-init-hook
  (evil-mode +1)
  (global-evil-surround-mode +1)
  (global-evil-visualstar-mode t))

;; {{ @see https://github.com/timcharper/evil-surround for tutorial
(defun evil-surround-prog-mode-hook-setup ()
  (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)
  (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
(add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)

(defun evil-surround-js-mode-hook-setup ()
  ;; ES6
  (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))
(add-hook 'js-mode-hook 'evil-surround-js-mode-hook-setup)

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

(defun evil-surround-org-mode-hook-setup ()
  (push '(93 . ("[[" . "]]")) evil-surround-pairs-alist) ; ]
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)

;; }}

;; {{ For example, press `viW*`
(setq evil-visualstar/persistent t)
;; }}

;; ffip-diff-mode (read only) evil setup
(defun ffip-diff-mode-hook-setup ()
  (evil-local-set-key 'normal "q" (lambda () (interactive) (quit-window t)))
  (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
  ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
  (evil-local-set-key 'normal "a" 'ffip-diff-apply-hunk)
  (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(with-eval-after-load 'evil
  (defmacro define-and-bind-text-object (key start-regex end-regex)
    (let* ((inner-name (make-symbol "inner-name"))
           (outer-name (make-symbol "outer-name")))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

  ;; between dollar signs:
  (define-and-bind-text-object "$" "\\$" "\\$")
  ;; between equal signs
  (define-and-bind-text-object "=" "=" "=")
  ;; between pipe characters:
  (define-and-bind-text-object "|" "|" "|")
  ;; regular expression
  (define-and-bind-text-object "/" "/" "/")
  ;; trimmed line
  (define-and-bind-text-object "l" "^ *" " *$")
  ;; angular template
  (define-and-bind-text-object "r" "\{\{" "\}\}")
  ;; }}

  ;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
  ;; You may delete this setup to use Evil NORMAL state always.
  (dolist (p '((minibuffer-inactive-mode . emacs)
               (calendar-mode . emacs)
               (special-mode . emacs)
               (grep-mode . emacs)
               (Info-mode . emacs)
               (term-mode . emacs)
               (sdcv-mode . emacs)
               (anaconda-nav-mode . emacs)
               (log-edit-mode . emacs)
               (vc-log-edit-mode . emacs)
               (magit-log-edit-mode . emacs)
               (erc-mode . emacs)
               (neotree-mode . emacs)
               (w3m-mode . emacs)
               (gud-mode . emacs)
               (help-mode . emacs)
               (eshell-mode . emacs)
               (shell-mode . emacs)
               (xref--xref-buffer-mode . emacs)
               ;;(message-mode . emacs)
               (epa-key-list-mode . emacs)
               (fundamental-mode . emacs)
               (weibo-timeline-mode . emacs)
               (weibo-post-mode . emacs)
               (woman-mode . emacs)
               (sr-mode . emacs)
               (profiler-report-mode . emacs)
               (dired-mode . emacs)
               (compilation-mode . emacs)
               (speedbar-mode . emacs)
               (ivy-occur-mode . emacs)
               (ffip-file-mode . emacs)
               (ivy-occur-grep-mode . normal)
               (messages-buffer-mode . normal)
               (js2-error-buffer-mode . emacs)
               (snails-mode . emacs)
               ))
    (evil-set-initial-state (car p) (cdr p)))

  ;; I prefer Emacs way after pressing ":" in evil-mode
  (lib-keys :map evil-ex-completion-map
    "C-a" 'move-beginning-of-line
    "C-b" 'backward-char
    "M-p" 'previous-complete-history-element
    "M-n" 'next-complete-history-element)
  (lib-keys :map evil-normal-state-map
    "Y" (kbd "y$")
    "go" 'goto-char)
  (lib-keys :map evil-insert-state-map
    "C-x C-n" 'evil-complete-next-line
    "C-x C-p" 'evil-complete-previous-line)
  (general-evil-setup t)

  ;; {{ use `,' as leader key
  (general-create-definer syl-leader-def
    :prefix ","
    :states '(normal visual))
  (syl-leader-def
    "bf" 'beginning-of-defun
    "bu" 'backward-up-list
    "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
    "ef" 'end-of-defun
    "m" 'evil-set-marker
    "eb" 'eval-buffer
    "ee" 'eval-expression
    "af" 'ace-delete-other-windows
    "aw" 'ace-swap-window
    "dj" 'dired-jump ;; open the dired from current file
    "xd" 'dired
    "xo" 'ace-window
    "ff" 'fullscreen-toggle ;; I use WIN-F in awesomewm
    "ip" 'find-file-in-project
    "jj" 'find-file-in-current-directory
    "tt" 'find-file-in-project-at-point
    "kk" 'find-file-in-project-by-selected
    "kn" 'find-file-with-similar-name ; ffip v5.3.1
    "fd" 'find-directory-in-project-by-selected
    "trm" 'shell-pop
    "tff" 'toggle-frame-fullscreen
    "tfm" 'toggle-frame-maximized
    ;; my emacs
    "xs" 'super-save-all-buffer
    "se" 'esup
    "so"  '(menu-item "" nil :filter
                      (lambda (&optional _)
                        (when (require 'restart-emacs nil t)
                          'restart-emacs)))
    "sb" 'core-benchmark/require-times
    ;; hydra-bundle
    "ub" 'hydra-ui-menu/body
    "hd" 'hydra-open-dir-menu/body
    "hs" 'one-key-tmp-scratch/menu
    "hf"  'one-key-functions/menu
    "ht" 'one-key-change-fontsize/menu
    "ua" 'one-key-adjust-opacity/menu)
  ;; }}
  ;; switch-windows
  (pcase lye-use-switch-windows-package
    ('ace-window
     (syl-leader-def
       "0" 'aw-select-window-0
       "1" 'aw-select-window-1
       "2" 'aw-select-window-2
       "3" 'aw-select-window-3
       "4" 'aw-select-window-4
       "5" 'aw-select-window-5
       "6" 'aw-select-window-6
       "7" 'aw-select-window-7
       "8" 'aw-select-window-8
       "9" 'aw-select-window-9))
    ('winum
     (syl-leader-def
       "0" 'winum-select-window-0-or-10
       "1" 'winum-select-window-1
       "2" 'winum-select-window-2
       "3" 'winum-select-window-3
       "4" 'winum-select-window-4
       "5" 'winum-select-window-5
       "6" 'winum-select-window-6
       "7" 'winum-select-window-7
       "8" 'winum-select-window-8
       "9" 'winum-select-window-9)))
  )
;; }}
