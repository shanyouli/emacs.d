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
(defhydra ui-menu (:exit nil)
  "GUI-MENU"
  ("t" tool-bar-mode "Tool-Bar")
  ("m" menu-bar-mode "Menu-Bar")
  ("s" scroll-bar-mode "Scroll-Bar")
  ("q" nil "quit"))

;; Common file shortcuts
;;;###autoload
(defhydra open-dir-menu (:exit t)
  "Open the hot folder."
  ("d" (lambda () (interactive) (lye/open-a-dir "~/.dotfiles")) "Dotfiles")
  ("e" (lambda () (interactive) (lye/open-a-dir user-emacs-directory)) "Emacs.d")
  ("g" (lambda () (interactive) (lye/open-a-dir "~/Git")) "Git Repo")
  ("r" (lambda () (interactive) (lye/open-a-dir "~/Dropbox")) "Dropbox")
  ("q" nil "quit"))
