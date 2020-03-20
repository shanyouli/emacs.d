;;; bundles/hydra/config.el -*- lexical-binding: t -*-

;;;###autoload
(cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
  "Add an icon in the hydra title."
  (let ((face (or face `(:foreground ,(face-background 'highlight))))
        (height (or height 1.0))
        (v-adjust (or v-adjust 0.0)))
    (concat
     (when (and (display-graphic-p) icon-type icon-name)
       (let ((f (intern (format "all-the-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
     (propertize title 'face face))))

;; The hot key about tool-bar, menu-bar, scroll-bar
;;;###autoload
(defhydra hydra-ui-menu (:exit nil)
  "GUI-MENU"
  ("t" tool-bar-mode "Tool-Bar")
  ("m" menu-bar-mode "Menu-Bar")
  ("s" scroll-bar-mode "Scroll-Bar")
  ("q" nil "quit"))

;; Common file shortcuts
;;;###autoload
(defhydra hydra-open-dir-menu (:exit t)
  "Open the hot folder."
  ("d" (lambda () (interactive) (lye/open-a-dir "~/.dotfiles")) "Dotfiles")
  ("e" (lambda () (interactive) (lye/open-a-dir user-emacs-directory)) "Emacs.d")
  ("g" (lambda () (interactive) (lye/open-a-dir "~/Git")) "Git Repo")
  ("r" (lambda () (interactive) (lye/open-a-dir "~/Dropbox")) "Dropbox")
  ("q" nil "quit"))

;; onekey func
(defonekey functions ()
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
  ("C" erase-all-buffer "Clear the entire buffer"))


;; tmp-scratch use key
(setq tmp-scratch-directory (lib-f-join lye-emacs-cache-dir "tmp-scratchs"))
(tmp-scratch-create-fun! 'orign)
(tmp-scratch-create-fun! 'emacs-lisp)
(tmp-scratch-create-fun! 'python)
(tmp-scratch-create-fun! 'shell)

(defonekey tmp-scratch nil
  "Temp file scratch"
  ("SPC" lib-scratch/orign "text")
  ("e" lib-scratch/emacs-lisp   "Elisp")
  ("p" lib-scratch/python   "Python")
  ("s" lib-scratch/shell   "Bash/zsh"))

;; font-adjust
(defonekey change-fontsize (nil t)
  "Font size."
  ("=" lib-font/increase-font-size "Inc font size")
  ("-" lib-font/decrease-font-size "Dec font size")
  ("0" lib-font/goto-default-size-font "Default font size"))

;; alpha adjust
(defonekey adjust-opacity (nil t)
  "Adjust opacity."
  ("-" (lambda () (interactive) (lye//adjust-opacity nil -2)) "Dec opacity")
  ("=" (lambda () (interactive) (lye//adjust-opacity nil 2))  "Inc opacity")
  ("+" (lambda () (interactive) (lye//adjust-opacity nil 2))  "Inc opacity")
  ("0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100))))
          "default opacity"))
