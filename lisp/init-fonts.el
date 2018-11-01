;;Font
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
;; 
(if (eq window-system 'w32)
    (set-font "Source Code Pro" "simsun" 14 16))

;;(set-font "Source Code Pro" "simsun" 14 16)
(provide 'init-fonts)
