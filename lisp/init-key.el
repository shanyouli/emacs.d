;;; init-key.el --- package.el keywords -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v1
;; Package-Requires: (lazy-load )
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: key


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

;; Key

;;; Code:

;; iex-elfeed
(lazy-load-global-keys '(("C-z w" . elfeed)) "iex-elfeed")

;; iex-pardox
(lazy-load-global-keys '(("C-z l" . lye/list-package)) "iex-paradox")

;; open line in browser
;; see @https://github.com/noctuid/link-hint.el/
(require-package 'link-hint)
(lazy-load-global-keys
 '(("C-x p o" . link-hint-open-link)
   ("C-x p c" . link-hint-copy-link))
 "link-hint")

(require-package 'org-cliplink)
(lazy-load-global-keys '(("C-x p i" . org-cliplink)) "org-cliplink")

(provide 'init-key)

;;; init-key.el ends here
