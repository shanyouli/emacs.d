;;; bundles/snails/config.el.el -*- lexical-binding: t -*-

;;; snails-backend-buffer-blacklist
(with-eval-after-load 'snails
  (when (fboundp 'fuz-build-and-load-dymod)
    (lib-load-absolute 'bundles/common/fuz-core))

  (dolist (buf (list
                " *which-key*"
              " *straight-process*"
              "*straight-process*"
              "*One-Key*"
              "*Flycheck**"
              "*flycheck-posframe-buffer*"
              " *flycheck-posframe-buffer*"
              " *company-posframe-buffer*"
              "*company"
              " *company"
              "*esup"
              " *pyim"
              " *server"
              " *sdcv"
              " *diff-hl*"
              " *snails"))
    (push buf snails-backend-buffer-blacklist)))

;;; snails-backend-themes
(defun snails-load-theme ()
  "Loading a theme use `snails'"
  (interactive)
  (require 'snails-backend-themes)
  (snails '(snails-backend-themes)))

;;; ido
(setq ido-enable-flex-matching t)                   ;模糊匹配
(setq ido-everywhere nil)                           ;禁用ido everyting, 拷贝操作不方便

(setq ido-use-filename-at-point 'guess
      ido-create-new-buffer     'always
      ido-max-prospects         10
      ido-save-directory-list-file (expand-file-name "ido.hist" lye-emacs-cache-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;; ido-sort-mtime
;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime) ;文件的排序方法
;; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)  ;目录的排序方法
;; smex
;; smex, remember recently and most frequently ised commands
(let ((amx-file (expand-file-name "amx-items" lye-emacs-cache-dir)))
  (if (file-exists-p amx-file)
      (setq smex-save-file amx-file)
    (setq smex-save-file (expand-file-name "smex-items" lye-emacs-cache-dir))))
(setq smex-history-length 10)

(add-hook! 'after-init-hook
    (ido-mode +1)
  (smex-initialize)
  (ido-sort-mtime-mode +1))
