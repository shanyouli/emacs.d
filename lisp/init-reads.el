;;; init-reads.el --- Initialize pdf-tools epub-mode -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: (pdf-tools nov-mode)
;; Homepage: homepage
;; Keywords: read-tools


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

;; commentary

;;; Code:

(when (display-graphic-p)
  ;; PDF View
  (use-package pdf-tools
    :ensure t
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward))
    :init
    (setq pdf-view-midnight-colors '("#ededed" . "#21242b")
          pdf-annot-activate-created-annotations t)
    :config
        ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when system/mac
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Recover last viewed position
    (when (< 26 emacs-major-version)
      (use-package pdf-view-restore
        :hook (pdf-view-mode . pdf-view-restore-mode)
        :init (setq pdf-view-restore-filename
                    (concat lye-emacs-cache-dir "pdf-view-restore")))))

  ;; Epub reader
  (use-package nov
    :ensure t
    :mode ("\\.[eE][pP][uU][bB]" . nov-mode)
    :preface
    ;; (defun my-nov-setup ()
    ;;   (visual-line-mode 1)
    ;;   (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    ;;   (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
    ;; :hook (nov-mode . my-nov-setup)
    :config
    (setq nov-save-place-file (concat lye-emacs-cache-dir "nov-places"))))
(provide 'init-reads)

;;; init-reads.el ends here
