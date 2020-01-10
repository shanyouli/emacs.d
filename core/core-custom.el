;;; core/core-custom.el -*- lexical-binding: t -*-

;;; Commentary
;; Custom-Variable

;;; Code

(defcustom lye-use-pyim-dictionary
  (if (and (not IS-WINDOWS) (executable-find "rime_dict_manager"))
      'librime 'base)
  "The value is base, big, librime.
If value is 'base, use pyim-basedict package.
If value is 'big, use pyim-bigdict package.
If value is 'librime, use liberime dynamic-module."
  :type '(choice
          (const :tag "Base dictionary" 'base)
          (const :tag "Big Dictionary" 'big)
          (const :tag "Liberime modules" 'librime)))

(defcustom lye-use-term-package
  (cond
    ((and module-file-suffix
           (executable-find "make")
           (executable-find "libtool")
           (executable-find "cmake"))
     'vterm)
    (IS-WINDOWS 'eshell)
    (t 'multi-term))
  "The vaule is vterm, eshell, multi-term.
If value is 'vterm, use vterm modules.
If value is 'multi-term, use multi-term package.
If value is nil, use eshell."
  :type '(choice
          (const :tag "vterm" 'vterm)
          (const :tag "Multi-term" 'multi-term)
          (const :tag "eshell" 'eshell)))

(defcustom lye-use-dict-package
  (cond ((executable-find "sdcv")
         'sdcv)
        (t 'ydcv))
  "The value is sdcv, ydcv, gdcv.
If value is 'sdcv, use sdcv packge.
If value is 'ydcv, use youdao-dictionary.
If value is 'gdcv, use google-translate."
  :type '(choice
          (const :tag "sdcv" 'sdcv)
          (const :tag "Youdao-dirctionary" 'ydcv)
          (const :tag "Google-translate" 'gdcv)))
