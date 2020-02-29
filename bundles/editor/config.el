;;; bundles/editor/config.el.el -*- lexical-binding: t -*-

(add-hook! 'after-init-hook
    ;; Big Blank delete
    (global-hungry-delete-mode +1)

  ;; Page Break lines
  (global-page-break-lines-mode +1))

;; Pair Automatic completion
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook! 'after-init-hook (electric-pair-mode +1))

;; Delete selection if you insert
(add-hook! 'after-init-hook (delete-selection-mode +1))

;; Display pair color
(add-hook! 'prog-mode-hook (rainbow-delimiters-mode +1))

;; Set blank highlight when use display graphic
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive t))
(add-hook! 'prog-mode-hook
    (if (< (line-number-at-pos (point-max)) 1000)
        (highlight-indent-guides-mode)))

;; color display
(add-hook! 'prog-mode-hook (rainbow-mode +1))

;; Extra blank hint
(with-eval-after-load 'whitespace
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))
(add-hook! 'prog-mode-hook (whitespace-mode +1))
(add-hook! 'outline-mode-hook (whitespace-mode +1))
(add-hook! 'conf-mode-hook (whitespace-mode +1))

;;; prettify-mode
(setq-default prettify-symbols-alist
              '(("<-" . "←")
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ("map" . ?↦)
                ("/=" . ?≠)
                ("!=" . ?≠)
                ("==" . ?≡)
                ("<=" . ?≤)
                (">=" . ?≥)
                ("=<<" . (?= (Br . Bl) ?≪))
                (">>=" . (?≫ (Br . Bl) ?=))
                ("<=<" . ?↢)
                (">=>" . ?↣)
                ("&&" . ?∧)
                ("||" . ?∨)))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook! 'prog-mode-hook (prettify-symbols-mode +1))

;; avy
(add-hook! 'after-init-hook (avy-setup-default))
(with-eval-after-load 'avy
  (ace-pinyin-global-mode +1)
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre))

;; lazy-search
(custom-set-faces
 '(lazy-search-highlight-current
   ((t :foreground "black" :background "orange" :bold t)))
 '(lazy-search-highlight-background
   ((t :foreground "gray" :background "black" :bold t))))

(with-eval-after-load 'rainbow-mode
  ;; Fix lazy-search and rainbow-mode conflicts
  (defvar lye-rainbow-mode-active-p nil
    "`rainbow-mode' is run or not.")
  (defun lye/rainbow-turn-off ($rest _)
    (when (and (featurep 'rainbow-mode) rainbow-mode)
      (setq lye-rainbow-mode-active-p t)
      (rainbow-mode -1)))
  (defun lye/rainbow-turn-on (&rest _)
    (when lye-rainbow-mode-active-p
      (setq lye-rainbow-mode-active-p nil)
      (rainbow-mode 1)))
  (advice-add #'lazy-search-quit :after #'lye/rainbow-turn-on)
  (advice-add #'lazy-search :after  #'lye/rainbow-turn-off))

;; backup-file
(when (fboundp 'backup-file)
  (setq backup-file-location (lib-f-join  lye-emacs-cache-dir "backup"))
  (add-hook! 'after-save-hook 'backup-file))
