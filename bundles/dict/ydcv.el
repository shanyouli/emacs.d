;;; bundles/dict/ydcv.el -*- lexical-binding: t -*-


(require 'youdao-dictionary)
;; Cache documents
(setq url-automatic-caching t)

;; Set file path for saving search history
(setq youdao-dictionary-search-history-file
      (concat lye-emacs-cache-dir "youdaohs"))

;; Enable Chinese word segmentation support (支持中文分词)
(setq youdao-dictionary-use-chinese-word-segmentation t)

(defun youdao-dictionary-search-at-point++ ()
  (interactive)
  (cond
   ((and (display-graphic-p) (> emacs-major-version 25))
    (call-interactively #'youdao-dictionary-search-at-point-posframe))
   ((display-graphic-p)
    (call-interactively #'youdao-dictionary-search-at-point-tooltip))
   (t
    (call-interactively #'youdao-dictionary-search-at-point))))
