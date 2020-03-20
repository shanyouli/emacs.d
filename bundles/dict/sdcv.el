;;; bundles/dict/sdcv.el -*- lexical-binding: t -*-

(require 'sdcv)

(let ((f1 (expand-file-name "sdcv" (or (getenv "XDG_DATA_HOME")
                                       (concat (getenv "HOME") "/.local/share"))))
      (f2 (expand-file-name "~/.stardict")))
  (setq sdcv-dictionary-data-dir (if (file-exists-p f1)
                                       f1
                                     f2)))

(setq sdcv-dictionary-simple-list ; setup dictionary list for simple search
      '("KDic11万英汉词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"))
(setq sdcv-dictionary-complete-list ; setup dictionary list for complete search
      '("KDic11万英汉词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "21世纪英汉汉英双向词典"
        "新世纪汉英科技大词典"
        "牛津现代英汉双解词典"
        "XDICT汉英辞典"
        "XDICT英汉辞典"
        "朗道汉英字典5.0"
        "朗道英汉字典5.0"
        "quick_eng-zh_CN"
        "CDICT5英汉辞典"))
(defun sdcv-search-at-point-tooltip ( &optional world)
  (interactive)
  (let ((word (sdcv-search-with-dictionary world sdcv-dictionary-simple-list)))
    (when word
      (require 'pos-tip)
      (pos-tip-show word nil nil nil 0))))

(defun sdcv-search-at-point++ ()
  (interactive)
  (cond
   ((and (display-graphic-p) (> emacs-major-version 25))
    (call-interactively #'sdcv-search-pointer+))
   ((display-graphic-p)
    (call-interactively #'sdcv-search-at-point-tooltip))
   (t
    (call-interactively #'sdcv-search-pointer))))
