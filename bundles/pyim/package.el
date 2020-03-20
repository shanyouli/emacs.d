;;; modules/pyim/package.el -*- lexical-binding: t -*-

(pcase lye-use-pyim-dictionary
  ('big (package! pyim-bigdict :recipe (
                    :repo "shanyouli/pyim-bigdict"
                    :host github
                    :files ( "*" ))))
  ('librime (package! liberime :recipe (
                        :repo "merrickluo/liberime"
                        :host github
                        :files ("CMakeLists.txt" "Makefile"
                                "src" "liberime.el")))))
