;;; bundles/ui/config.el.el -*- lexical-binding: t -*-

(require 'lib-modeline nil t)
(defvar lye-default-modeline-format mode-line-format)
(pcase lye-use-modeline
  ('base (message "Other modeline settings are not loaded..."))
  ('doom (lib-load-relative "doom-modeline" t t))
  ('awetray (lib-load-relative "awesome-tray" t t)
            (lib-load-relative "awesome-tab" t t)))
