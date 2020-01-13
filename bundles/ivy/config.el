;;; bundles/ivy/config.el -*- lexical-binding: t -*-

(add-hook! 'after-init-hook (ivy-mode +1))
(add-hook! 'ivy-mode-hook (counsel-mode +1))

(setq ivy-use-selectable-prompt t) ;; Allow commands in minibuffers

(setq ivy-use-selectable-prompt t
      ivy-use-virtual-buffers t  ;; Enable bookmarks and recentf
      ivy-height 10
      ivy-fixed-height-minibuffer t
      ivy-count-format "(%d/%d) "
      ivy-on-del-error-function nil
      ivy-initial-inputs-alist nil)

(setq swiper-action-recenter t)

(setq counsel-find-file-at-point t
      counsel-yank-pop-separator "\n────────\n")

;; Use the faster search tool: ripgrep (`rg')
(when (executable-find "rg")
  (setq counsel-grepbase-command "rg -S --no-heading --line-number --color never %s %s")
  (when (and IS-MAC (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

;; Use the faster search tool: skim(`sk')

;; (when (executable-find "sk")
;;   (setq counsel-fzf-cmd "sk --no-sort -f \"%s\""))

(with-eval-after-load 'counsel
  (with-no-warnings
    ;; Display an arrow with the selected item
    (defun my-ivy-format-function-arrow (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat (if (and (bundle-active-p 'icons)
                          (require 'all-the-icons nil t))
                     (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                   ">")
                 (propertize " " 'display `(space :align-to 2))
                 (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat (propertize " " 'display `(space :align-to 2)) str))
       cands
       "\n"))
    (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

        ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
        flush-lines keep-lines ivy-read
        swiper swiper-backward swiper-all
        swiper-isearch swiper-isearch-backward
        lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
        counsel-grep-or-swiper counsel-grep-or-swiper-backward
        counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
    (defvar-local my-ivy-fly--travel nil)

    (defun my-ivy-fly-back-to-present ()
      (cond ((and (memq last-command my-ivy-fly-commands)
                  (equal (this-command-keys-vector) (kbd "M-p")))
             ;; repeat one time to get straight to the first history item
             (setq unread-command-events
                   (append unread-command-events
                           (listify-key-sequence (kbd "M-p")))))
            ((or (memq this-command '(self-insert-command
                                      ivy-forward-char end-of-line mwim-end-of-line
                                      mwim-end-of-code-or-line mwim-end-of-line-or-code
                                      yank ivy-yank-word counsel-yank-pop))
                 (equal (this-command-keys-vector) (kbd "M-n")))
             (unless my-ivy-fly--travel
               (delete-region (point) (point-max))
               (when (memq this-command '(ivy-forward-char
                                          end-of-line mwim-end-of-line
                                          mwim-end-of-code-or-line
                                          mwim-end-of-line-or-code ))
                 (insert (ivy-cleanup-string ivy-text)))
               (setq my-ivy-fly--travel t)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (let* ((kbd (kbd "M-n"))
               (cmd (key-binding kbd))
               (future (and cmd
                            (with-temp-buffer
                              (when (ignore-errors
                                      (call-interactively cmd) t)
                                (buffer-string))))))
          (when future
            (save-excursion
              (insert (propertize (replace-regexp-in-string
                                   "\\\\_<" ""
                                   (replace-regexp-in-string
                                    "\\\\_>" ""
                                    future))
                                  'face 'shadow)))
            (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
    (add-hook 'minibuffer-exit-hook
              (lambda ()
                (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

        ;; Improve search experience of `swiper' and `counsel'
    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (counsel-rg ivy-text default-directory)
          (swiper-isearch ivy-text))))

    ;; Intergration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))
    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy)))

    ;; amx
  (setq amx-history-length 20
        amx-save-file
        (let ((smex-file (lib-f-join lye-emacs-cache-dir "smex-items")))
          (if (file-exists-p smex-file)
              smex-file
            (lib-f-join lye-emacs-cache-dir "amx-items")))))

;; More friendly display transformer for Ivy
(when (bundle-active-p 'icons)
  (lib-load-relative "ivy-rich"))
