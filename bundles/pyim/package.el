;;; modules/pyim/package.el -*- lexical-binding: t -*-

(pcase lye-use-pyim-dictionary
  ('big (package+ '(pyim-bigdict
                    :repo "shanyouli/pyim-bigdict"
                    :host github
                    :files ( "*" ))))
  ('librime (package+ '(liberime
                        :repo "merrickluo/liberime"
                        :host github
                        :files ("CMakeLists.txt" "Makefile"
                                "src" "liberime-config.el")))))
