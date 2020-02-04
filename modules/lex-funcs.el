;;; lex-funcs.el --- Define functions.              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  DESKTOP-RD96RHO

;; Author: DESKTOP-RD96RHO <lye@DESKTOP-RD96RHO>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define functions.

;;; Code



;; hydra key
(defhydra hydra-functions-menu (:exit t)
  "Functions Menu"
  ("d" dos2unix "Dos2Unix")
  ("u" unix2dos "Unix2dos")
  ("r" revert-current-buffer "Revert current buffer")
  ("s" save-buffer-as-utf-8 "Save as utf-8")
  ("g" save-buffer-with-gbk "Save as GBK")
  ("c" revert-buffer-with-gbk "Revert Buffer with GBK")
  ("i" revert-buffer-with-utf-8 "Revert Buffer with utf8")
  ("m" recompile-elpa "Recompile elpa")
  ("n" rename-this-file-and-buffer "Rename File")
  ("b" browse-current-file "Browse current File")
  ("f" sudo-find-file "find file as Root")
  ("u" sudo-this-file "Open file as root")
  ("C" erase-all-buffer "Clear the entire buffer")
  ("q" nil "quit"))

(provide 'lex-funcs)
;;; lex-funcs.el ends here
