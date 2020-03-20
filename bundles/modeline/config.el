;;; bundles/ui/config.el.el -*- lexical-binding: t -*-
(lye-load! "base-modeline" nil t t)
(setq lye-init--modeline-format mode-line-format)

(pcase lye-use-modeline
  ('doom (lye-load! "doom-modeline" nil t t))
  ('awetray (lye-load! "awesome-tray" nil t t)
            (lye-load! "awesome-tab" nil t t)))
