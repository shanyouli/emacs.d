;;; bundles/dict/config.el -*- lexical-binding: t -*-


(defalias 'lye/dict-point
    (pcase lye-use-dict-package
      ('sdcv (lib-load-relative "sdcv" t t)
             #'sdcv-search-at-point++)
      ('ydcv (lib-load-relative "ydcv" t t)
             #'youdao-dictionary-search-at-pinit++)))
