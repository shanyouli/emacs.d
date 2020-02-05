;;; bundles/editor/package.el.el -*- lexical-binding: t -*-


(package! 'rainbow-delimiters :commands rainbow-delimiters-mode)
(package! 'rainbow-mode :commands rainbow-mode)

(package! 'hungry-delete :commands global-hungry-delete-mode)
(package! 'highlight-indent-guides :commands highlight-indent-guides-mode)
(package! 'page-break-lines :commands global-page-break-lines-mode)

(package! 'elec-pair :local t :commands electric-pair-mode)

(package! 'delsel :local t :commands delete-selection-mode)

;; Chinese input automatically adds spaces in Chinese
;; (package! 'pangu-spacing :commands pagu-spacing-mode)

(package! 'whitespace :commands witespace-mode :local t)

(package! '(smart-align :type git :host github :repo "manateelazycat/smart-align"
            :no-byte-compile t) :commands smart-align)

(package! 'thing-edit :local t
          :commands (thing-copy-word
                     thing-copy-symbol
                     thing-copy-filename
                     thing-copy-sexp
                     thing-copy-page
                     thing-copy-list
                     thing-copy-defun
                     thing-copy-parentheses
                     thing-copy-region-or-line
                     thing-copy-to-line-beginning
                     thing-copy-to-line-end

                     thing-cut-word
                     thing-cut-symbol
                     thing-cut-filename
                     thing-cut-sexp
                     thing-cut-page
                     thing-cut-list
                     thing-cut-defun
                     thing-cut-parentheses
                     thing-cut-region-or-line
                     thing-cut-to-line-beginning
                     thing-cut-to-line-end))

;; avy
(package! 'avy :commands avy-setup-default)
(package! 'ace-pinyin :commands ace-pinyin-global-mode)
