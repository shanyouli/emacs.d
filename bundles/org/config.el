;;; bundles/org/config.el.el -*- lexical-binding: t -*-
(add-hook! 'org-mode-hook
    ;; (auto-fill-mode nil) ; 不自动换行
    (setq truncate-lines nil) ; 不自动换行

  ;; Beautify Org Checkbox Symbol
  ;; @seehttps://github.com/seagle0128/.emacs.d/blob/master/lisp/md-org.el#L44
  (when (char-displayable-p ?☐)
    (push '("[ ]" . "☐") prettify-symbols-alist))
  (when (char-displayable-p ?☑)
    (push '("[X]" . "☑") prettify-symbols-alist))
  (when (char-displayable-p ?❍)
    (push '("[-]" . "❍") prettify-symbols-alist))
  (when (char-displayable-p ?λ)
    (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
    (push '("#+END_SRC" . "λ") prettify-symbols-alist))
  (prettify-symbols-mode +1))

(when (fboundp 'org-bullets-mode)
  (add-hook! 'org-mode-hook (org-bullets-mode +1))
  (with-eval-after-load 'org-bullets
    (setq org-bullets-bullet-list
          '("◯" "◉" "◌" "⚈" "☉" "✥" "✡" "✤" "✣" "✴"))))

;; 自动缩进，* or ** etc..
(add-hook! 'org-mode-hook (org-indent-mode +1))
(add-hook! 'org-indent-hook
    ;; @see https://github.com/seagle0128/.emacs.d/issues/88
    (make-variable-buffer-local 'show-paren-mode)
  (setq-local show-paren-mode nil))
(with-eval-after-load 'org
  ;; org-bable
  ;; Don't ask to eval code in SRC blocks.
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)
                               (shell . t)))

  (cl-pushnew '(go . t) load-language-list)

  (cl-pushnew '(rust . t) load-language-list)

  (when (executable-find "jupyter")
    (cl-pushnew '(ipython . t) load-language-list))
  (org-babel-do-load-languages 'org-babel-load-languages
                             load-language-list)
  (require 'htmlize nil t))

(with-eval-after-load 'ox
  (require 'ox-hugo nil t)
  (setq org-hugo-section "post")
  (require 'easy-hugo nil t))
