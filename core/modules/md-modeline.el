;;; md-modeline.el --- Initialize Modeline Module -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.1
;; Package-Requires: ()
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: modeline
;; Last-Updated: 2019-11-17 17:13:01


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

;;; Change log:
;;
;; 11/19/19
;;        * 添加留白, 参考: https://emacs-china.org/t/mode-line/8656
;; 11/18/19
;;        * add `md-modeline-window-number'
;;
;; 11/17/19
;;        * initialize modeline
;;        * copy from `mood-line'

;;; Code:
;;
;; Variable declarations
;;

(defvar flycheck-current-errors)
(defvar flymake--mode-line-format)
(defvar anzu--state)
(defvar anzu--cached-count)
(defvar anzu--overflow-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar multiple-cursors-mode)

;;
;; Function prototypes
;;

(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function mc/num-cursors "multiple-cursors" ())
;;
;; Config
;;

(defgroup md-modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom md-modeline-show-eol-style nil
  "If t, the EOL style of the current buffer will be displayed in the mode-line."
  :group 'md-modeline
  :type 'boolean)

(defcustom md-modeline-show-encoding-information nil
  "If t, the encoding format of the current buffer will be displayed in the mode-line."
  :group 'md-modeline
  :type 'boolean)

(defcustom md-modeline-show-cursor-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'md-modeline
  :type 'boolean)

(defface md-modeline-status-neutral
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'md-modeline)

(defface md-modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'md-modeline)

(defface md-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'md-modeline)

(defface md-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'md-modeline)

(defface md-modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'md-modeline)

(defface md-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'md-modeline)

(defface md-modeline-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'md-modeline)

(defface md-modeline-window-number
  '((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))
  "Face use for the `window number'."
  :group 'md-modeline)

;;
;; Helper functions
;;

(defun --string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun --string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun --string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (--string-trim-left (--string-trim-right string)))

(defun --format-md-modeline (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat left
            (propertize " "
                        'display '(height 1.4))
            (propertize " " 'display '(raise -0.3))
            (propertize " "
                        'display `((space :align-to (- right ,reserve))))
            right)))

;;
;; Update functions
;;

(defvar-local md-modeline--vc-text nil)
(defun md-modeline--update-vc-segment (&rest _)
  "Update `md-modeline--vc-text' against the current VCS state."
  (setq md-modeline--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'md-modeline-status-info)
                             (propertize "+ " 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'md-modeline-status-warning)
                             (propertize "⟷ " 'face face))
                            ((eq state 'needs-update)
                             (setq face 'md-modeline-status-warning)
                             (propertize "↑ " 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'md-modeline-status-error)
                             (propertize "✖ " 'face face))
                            (t
                             (setq face 'md-modeline-status-neutral)
                             (propertize "✔ " 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      "  "))))))

(defvar-local md-modeline--flycheck-text nil)
(defun md-modeline--update-flycheck-segment (&optional status)
  "Update `md-modeline--flycheck-text' against the reported flycheck STATUS."
  (setq md-modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'md-modeline-status-error
                                                 'md-modeline-status-warning))))
                       (propertize "✔ Good  " 'face 'md-modeline-status-success)))
          ('running (propertize "Δ Checking  " 'face 'md-modeline-status-info))
          ('errored (propertize "✖ Error  " 'face 'md-modeline-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'md-modeline-status-neutral))
          ('no-checker ""))))

;;
;; Segments
;;

(defun md-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (if (buffer-modified-p)
          (propertize "● " 'face 'md-modeline-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "■ " 'face 'md-modeline-unimportant)
          "  "))
    "  "))

(defun md-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize "%b  " 'face 'mode-line-buffer-id))

(defun md-modeline-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (cond ((eq anzu--state 'replace-query)
           (format #("Replace: %d  " 0 11 (face md-modeline-status-warning)) anzu--cached-count))
          (anzu--overflow-p
           (format #("%d/%d+  " 0 3 (face md-modeline-status-info) 3 6 (face md-modeline-status-error)) anzu--current-position anzu--total-matched))
          (t
           (format #("%d/%d  " 0 5 (face md-modeline-status-info)) anzu--current-position anzu--total-matched)))))

(defun md-modeline-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat "MC"
            (format #("×%d  " 0 3 (face md-modeline-status-warning)) (mc/num-cursors)))))

(defun md-modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when md-modeline-show-cursor-point (propertize (format ":%d" (point)) 'face))
          (propertize " %p%%  " 'face 'md-modeline-unimportant)))

(defun md-modeline-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (when md-modeline-show-eol-style
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF  ")
      (1 "CRLF  ")
      (2 "CR  "))))

(defun md-modeline-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (when md-modeline-show-encoding-information
    (concat (let ((sys (coding-system-plist buffer-file-coding-system)))
              (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name (plist-get sys :name))))))
            "  ")))

(defun md-modeline-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  md-modeline--vc-text)

(defun md-modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  " 'face 'bold))

(defun md-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (format-mode-line mode-line-misc-info 'md-modeline-unimportant)))
    (unless (string= misc-info "")
      (concat (--string-trim misc-info) "  "))))

(defun md-modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  md-modeline--flycheck-text)

(defun md-modeline-segment-flymake ()
  "Displays information about the current status of flymake in the mode-line (if available)."
  (when (and (boundp 'flymake-mode) flymake-mode)
    (concat (--string-trim (format-mode-line flymake--mode-line-format)) "  ")))

(defun md-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (concat (--string-trim (format-mode-line mode-line-process)) "  ")))


(declare-function window-numbering-clear-mode-line 'window-numbering)
(declare-function window-numbering-get-number-string 'window-numbering)
(declare-function window-numbering-install-mode-line 'window-numbering)
(declare-function winum--clear-mode-line 'winum)
(declare-function winum--install-mode-line 'winum)
(declare-function winum-get-number-string 'winum)

(defun md-modeline-segment-window-number ()
  "Displays the current window number."
  (let ((num (cond
              ((bound-and-true-p ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              ((bound-and-true-p winum-mode)
               (setq winum-auto-setup-mode-line nil)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
    (if (and (< 0 (length num))
             (< (if (active-minibuffer-window) 2 1)
                (length (cl-mapcan #'window-list (visible-frame-list)))))
        (concat " "
                (propertize (format "%s" num) 'face 'md-modeline-window-number)
                " "))))

;;
;; Activation function
;;

;; Store the default mode-line format
(defvar md-modeline--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode md-modeline-mode
  "Toggle md-modeline on or off."
  :group 'md-modeline
  :global t
  :lighter nil
  (if md-modeline-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'md-modeline--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'md-modeline--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'md-modeline--update-vc-segment)
        (add-hook 'after-save-hook #'md-modeline--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'md-modeline--update-vc-segment)

        ;; window-number
        (advice-add #'window-numbering-install-mode-line :override #'ignore)
        (advice-add #'window-numbering-clear-mode-line :override #'ignore)
        (advice-add #'winum--install-mode-line :override #'ignore)
        (advice-add #'winum--clear-mode-line :override #'ignore)
        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '(
                        (:eval
                         (--format-md-modeline
                          ;; Left
                          (format-mode-line
                           '(" "
                             (:eval (md-modeline-segment-window-number))
                             (:eval (md-modeline-segment-modified))
                             (:eval (md-modeline-segment-buffer-name))
                             (:eval (md-modeline-segment-anzu))
                             (:eval (md-modeline-segment-multiple-cursors))
                             (:eval (md-modeline-segment-position))))

                          ;; Right
                          (format-mode-line
                           '((:eval (md-modeline-segment-eol))
                             (:eval (md-modeline-segment-encoding))
                             (:eval (md-modeline-segment-vc))
                             (:eval (md-modeline-segment-major-mode))
                             (:eval (md-modeline-segment-misc-info))
                             (:eval (md-modeline-segment-flycheck))
                             (:eval (md-modeline-segment-flymake))
                             (:eval (md-modeline-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'md-modeline--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'md-modeline--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'md-modeline--update-vc-segment)
      (remove-hook 'after-save-hook #'md-modeline--update-vc-segment)
      (advice-remove #'vc-refresh-state #'md-modeline--update-vc-segment)

      ;; Remove winum
      (advice-remove #'window-numbering-install-mode-line #'ignore)
      (advice-remove #'window-numbering-clear-mode-line #'ignore)
      (advice-remove #'winum--install-mode-line  #'ignore)
      (advice-remove #'winum--clear-mode-line  #'ignore)
      ;; Restore the original mode-line format
      (setq-default mode-line-format md-modeline--default-mode-line))))

;;
;; Provide md-modeline
;;

(provide 'md-modeline)

;;; md-modeline.el ends here
