;;; autoload/initfont.el -*- lexical-binding: t; -*-
(defadvice! my/use-default-font-a (&rest _)
  "Set `doom-font'!"
  :before #'doom-init-fonts-h
  (cl-loop for font in '("PragmataPro Liga" "Cascadia Code" "Fantasque Sans Mono")
           when (doom-font-exists-p font)
           return (setq doom-font (font-spec :family font :size 13)))
  (unless doom-font
    (cl-loop for font in '("JetBrains Mono" "Fira Code" "Source Code Pro" "Menlo" "monospace")
             when (doom-font-exists-p font)
             return (setq doom-font (font-spec :family font :size 12))))
  (advice-remove #'doom-init-fonts-h #'my/use-default-font-a))

(defadvice! my/use-chinese-font-a (&rest _)
  "Set Chinese fonts"
  :after #'doom-init-fonts-h
  (cl-loop for font in '("LXGW WenKai Mono" "Adobe Heiti Std" "STXihei" "Microsoft Yahei"
                         "Hiragino Sans GB W6" "WenQuanYi Micro Hei Mono")
           when (and (doom-font-exists-p font) (find-font (font-spec :family (font-get doom-font :family) :script 'han)))
           return (dolist (charset '(kana han cjk-misc bopomofo))
                    (set-fontset-font t charset font)))
  ;; org modern header 字体配置
  (when (doom-font-exists-p "Unifont")
    (set-fontset-font t '(#x262f . #x2637) "Unifont")
    (set-fontset-font t '(#x2460 . #x2468) "Unifont")))
;; ligatures
(when (modulep! :ui ligatures +extra)
  (plist-put! +ligatures-extra-symbols :pipe "‖")
  (add-hook 'after-setting-font-hook
            (lambda ()
              (when (and (display-graphic-p)
                         (string-equal (font-get doom-font :family) "Fantasque Sans Mono"))
                (set-fontset-font t '(#X03bb . #X03bb) "Fantasque Sans Mono") ;; :lambda
                (set-fontset-font t '(#X2022 . #X2022) "Fantasque Sans Mono")) ;; dot

              (cl-loop for font in '("STIX Two Math" "Latin Modern Math")
                       when (doom-font-exists-p font)
                       return (dolist (charset (list #X2218 ;; composition
                                                     #X21a6 ;; map
                                                     #X2205 ;; null
                                                     #X1d54b ;; true
                                                     #X1d53d ;; false
                                                     #X2124 ;; int
                                                     #X211d ;; float
                                                     #X1d54a ;; str
                                                     #X1d539 ;; bool
                                                     #X1d543 ;; list
                                                     #X22c3 ;; union
                                                     #X2229 ;; intersect
                                                     #X2216 ;; diff
                                                     #X2a02 ;; tuple
                                                     ))
                                ;; (set-fontset-font t (cons charset charset) font)
                                (set-fontset-font t `(,charset . ,charset) font))))))

(defun my-ligatures-init-buffer-h ()
  (when after-init-time
    (let ((in-mode-extras-p (+ligatures--enable-p +ligatures-extras-in-modes)))
      (when in-mode-extras-p
        (prependq! prettify-symbols-alist
                   (alist-get major-mode +ligatures-extra-alist)))
      (when (and in-mode-extras-p
                 prettify-symbols-alist)
        (when prettify-symbols-mode
          (prettify-symbols-mode -1))
        (prettify-symbols-mode +1)))))
(when (and (modulep! :ui ligatures)
           (not (modulep! :ui ligatures +extra)))
  (add-hook! 'doom-init-ui-hook :append
    (defun my-ligatures-init-h ()
      (add-hook 'after-change-major-mode-hook #'my-ligatures-init-buffer-h))))

;;;###autoload
(defun my-init-font () "init my chinese and english font")
