;;; modules/pyim/package.el -*- lexical-binding: t -*-

(defvar modules-pyim-use-dict 'base
  "The value is base, big, librime.
If value is 'base, use pyim-basedict package.
If value is 'big, use pyim-bigdict package.
If value is 'librime, use liberime dynamic-module.")

(package+ 'pyim)

(pcase modules-pyim-use-dict
  ('base (package+ 'pyim-basedict))
  ('big (package+ '(pyim-bigdict
                    :repo "shanyouli/pyim-bigdict"
                    :host github
                    :files ( "*" ))))
  ('librime (package+ '(liberime
                        :repo "merrickluo/liberime"
                        :host github
                        :files ("CMakeLists.txt" "Makefile"
                                "src" "liberime-config.el")))))
