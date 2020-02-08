;;; bundles/ui/config.el.el -*- lexical-binding: t -*-

(defvar lye-default-modeline-format mode-line-format)
(pcase lye-use-modeline
  ('base (require 'lib-modeline nil t))
  ('doom (lib-load-relative "doom-modeline" t t))
  ('awetray (lib-load-relative "awesome-tray" t t)
            (lib-load-relative "awesome-tab" t t)))
