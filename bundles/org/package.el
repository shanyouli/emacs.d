;;; bundles/org/package.el.el -*- lexical-binding: t -*-


(package! org :commands (org-mode
                          org-agenda
                          org-store-link
                          org-switchb)
          :mode ("\\.org$\\'" . org-mode))

(package! org-bullets :if (char-displayable-p ?◉)
          :commands (org-bullets-mode))

(package! org-indent :commands org-indent-mode :build-in t)

(package! org-babel :build-in t)

(package! ob-go :defer t)

(package! ob-rust :defer t)

(package! ob-ipython :if (executable-find "jupyter") :defer t)

(package! htmlize)

(package! ox-hugo)
(package! easy-hugo)

(package! org-cliplink :commands org-cliplink)
