;;; /core/core-bundle.el -*- lexical-binding: t -*-


(bundle! pyim :defer t)
;; (bundle! fcitx :defer t :if (and IS-LINUX (executable-find "fcitx-remote")))

(bundle! term :defer t)

(bundle! icons :if (and (display-graphic-p) (not IS-WINDOWS)))

(bundle! dict :commands lye/dict-point)

(bundle! hydra
  :commands (pretty-hydra-title
             hydra-ui-menu/body
             hydra-open-dir-menu/body
             one-key-functions/menu
             one-key-tmp-scratch/menu
             one-key-change-fontsize/menu
             one-key-adjust-opacity/menu))

(bundle! window)

(bundle! elisp :defer t)

(pcase lye-use-search-frame
  ('snails (bundle! snails))
  ('ivy (bundle! ivy)))

(bundle! rss :key elfeed-hydra/body)

(bundle! company)

(bundle! dired)

(bundle! mode :defer t)

(bundle! yasnippet)

(bundle! lsp :defer t)

(bundle! flycheck :defer t)

(bundle! editor :key one-key-thing-edit/menu)

(bundle! tools)

(bundle! treemacs)

(bundle! git :defer t :key one-key-magit/menu)

;; DONE: Use bundle replace modules
(setq lye-use-modeline 'awetray)
(bundle! modeline)

(bundle! org :defer 0.5)

(bundle! pdf :defer 0.5)
