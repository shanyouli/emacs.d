;;; bundles/ui/config.el.el -*- lexical-binding: t -*-

(pcase lye-use-modeline
  ('base (lib-load-relative "base-modeline" t t))
  ('doom (lib-load-relative "doom-modeline" t t))
  ('awetray (lib-load-relative "awesome-tray" t t)
            (lib-load-relative "awesome-tab" t t)))
