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
