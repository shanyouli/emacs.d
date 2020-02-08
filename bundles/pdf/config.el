;;; bundles/pdf/config.el.el -*- lexical-binding: t -*-

(defun lye//pdf-tools-check ()
  (let ((epdfinfo-file (locate-library "epdfinfo"))
        (lye--pdf-tools (lib-f-join straight-dynamic-modules-dir "epdfinfo")))
    (cond
      ((file-exists-p lye--pdf-tools)
       (setq pdf-info-epdfinfo-program lye--pdf-tools))
      ((and epdfinfo-file (not (string= lye--pdf-tools epdfinfo-file)))
       (lye//move-file pdf-info-epdfinfo-program lye--pdf-tools)
       (setq pdf-info-epdfinfo-program lye--pdf-tools))
      (t
       (pdf-tools-install t nil t t)
       (advice-add 'save-buffer-kill-emacs
                   :before
                   (lambda (&rest _)
                     (when (file-exists-p pdf-info-epdfinfo-program)
                       (lye//move-file pdf-info-epdfinfo-program
                                       lye--pdf-tools))))))))

(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
(setq pdf-view-midnight-colors '("#ededed" . "#21242b")
      pdf-annot-activate-created-annotations t)
(with-eval-after-load 'pdf-view
  ;; WORKAROUND: Fix compilation errors on macOS.
  ;; @see https://github.com/politza/pdf-tools/issues/480
  (when IS-MAC
    (setenv "PKG_CONFIG_PATH"
            "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
  (lye//pdf-tools-check))

(with-eval-after-load 'nov
  (setq nov-save-place-file (concat lye-emacs-cache-dir "nov-places")))
