;;; bundles/dict/config.el -*- lexical-binding: t -*-


(defalias 'lye/dict-point
    (pcase lye-use-dict-package
      ('sdcv (lye-load! "sdcv" nil t t)
             #'sdcv-search-at-point++)
      ('ydcv (lye-load! "ydcv" nil t t)
             #'youdao-dictionary-search-at-pinit++)))
