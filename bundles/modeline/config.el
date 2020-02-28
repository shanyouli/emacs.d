;;; bundles/ui/config.el.el -*- lexical-binding: t -*-
(lib-load-relative "base-modeline" t t)
(setq lye-init--modeline-format mode-line-format)

(pcase lye-use-modeline
  ('doom (lib-load-relative "doom-modeline" t t))
  ('awetray (lib-load-relative "awesome-tray" t t)
            (lib-load-relative "awesome-tab" t t)))
