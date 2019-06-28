;;;; init-font.el ---Initalize Font Management        -*- lexical-binding: t; -*-

;;;; Code

(defvar emacs-english-font nil
  "The font name of English.")
(defvar emacs-cjk-font nil
  "The font name of CJK.")
(defvar emacs-font-size-pair nil
  "Default font size pair for (english . chinese)")
(defvar emacs-font-size-pair-list nil
  "This is used to store matching (english . chinese font-size).
Corresponding English font pixelsize is 12px, 14px, 16px, 19px, 21px, 22px,
24px, 29px, 32px, 35px, (40px), 48px, (56px).
Corresponding English font size is 9pt, 10.5pt, 12pt, 15pt, 16pt, 18pt, 22pt,
 24pt, 26pt, (30pt), 36pt, (42pt)
使用中文字号分别对应为: 小五, 五号, 小四, 四号, 小三, 三号, 小二, 二号, 小一,
一号, (30pt), 小初, (初号)")

;;;  Functions
(defun font-exist-p (fontname)
  "Test this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil
      t)))

;; see @https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bintl/chinese/config.el#L26
(defun lye/set-monospaced-font (english chinese size-pair)
  "Set the monospaced font size when mixed Chinese and English words"
  (set-face-attribute 'default nil :font
                      (font-spec :family english :size (car size-pair)))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size (cdr size-pair)))))


;; see @https://gist.github.com/Superbil/7113937#file-fix-font-org-mode-el-L31
(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0)
        (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "Emacs font size set to %.1f" (car emacs-font-size-pair))
      (lye/set-monospaced-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (emacs-step-font-size -1))

(when (display-graphic-p)
  ;; CJK font set
  (catch 'loop
    (dolist (font '("WenQuanYi Micro Hei"
                    "Hiragino Sans GB"
                    "Microsoft Yahei"
                    "楷体"
                    "Noto Sans Mono CJK SC"))
      (when (font-exist-p font)
        (setq emacs-cjk-font font)
        (throw 'loop t))))

  ;; English font set
  (catch 'loop
    (dolist (font '("Fantasque Sans Mono"
                    "Fira Code"
                    "Source Code Pro"
                    "Hack"
                    "DejaVu Sans Mono"
                    "Consolas"))
      (when (font-exist-p font)
        (setq emacs-english-font font)
        (throw 'loop t))))

  ;; Font size and font size up or down configuration
  (cond
   ((string= emacs-english-font "Fantasque Sans Mono")
    (setq emacs-font-size-pair '(14 . 14))
    (setq emacs-font-size-pair-list
          '((12 . 12) (14 . 14) (16 . 16) (19 . 20)
            (21 . 22) (22 . 22) (24 . 24) (29 . 30)
            (32 . 34) (35 . 36) (48 . 50) (56 . 56)
            )))
   (t
    ;; Applicable fonts have Fira Code, Hack, Source Code Pro
    (setq emacs-font-size-pair '(14 . 16))
    (setq emacs-font-size-pair-list
          '((12 . 14) (14 . 16) (16 . 20) (19 . 22)
            (21 . 26) (22 . 26) (24 . 28) (29 . 34)
            (32 . 38) (35 . 42) (40 . 48) (48 . 56)
            ))
    ))
  ;;(setq emacs-english-font "Source Code Pro")
  (lye/set-monospaced-font emacs-english-font
                           emacs-cjk-font
                           emacs-font-size-pair)

  ;; setup change size font, base on emacs-font-size pair-list
  (global-set-key (kbd "C-M-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C-M--") 'decrease-emacs-font-size)

    ;; Specify font for all unicode characters
  (catch 'loop
    (dolist (font '("Symbola" "Apple Symbols" "Symbol"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (throw 'loop t)))))

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

(provide 'init-font)
;;; init-font.el ends here
