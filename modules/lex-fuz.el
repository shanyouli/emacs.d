;;; lex-fuz.el --- Initialize fuz -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1 
;; Package-Requires: (fuz)
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: fuzzy


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;  provides some fuzzy match scoring/matching functions for Emacs

;;; Code:

(with-eval-after-load 'ivy
  (require 'ivy-fuz)
  (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))

  (add-to-list 'ivy-highlight-functions-alist
               '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(provide 'lex-fuz)

;;; lex-fuz.el ends here
