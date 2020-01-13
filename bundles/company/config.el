;;; bundles/company/config.el -*- lexical-binding: t -*-

(add-hook! 'after-init-hook (global-company-mode +1))

(defun company-backend-with-yas (backend)
  "Use `company-yasnippet' as the complement backend for `company'."
  (if (or (not lye-company-enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)
               (not (member 'company-tabnine backend))))
      backend
    (if (listp backend)
        (let ((f (car backend))
              (s (cdr backend)))
          (append (if (listp f)
                      (append f '(with company-yasnippet))
                    (list f :with company-yasnippet))
                  s))
      (list backend :with company-yasnippet))))

(defun lye-company-yasnippet ()
  "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))

(with-eval-after-load 'company
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        ;; company-global-modes '(not erc-mode message-mode help-mode gud-mode
        ;; eshell-mode shell-mode)
        ;; company-backends '(company-capf)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))


  ;; Icons and quickhelp
  (add-hook! 'company-mode-hook (company-box-mode +1))
  (setq company-box-backends-colors nil
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-doc-delay 0.5)
  (with-eval-after-load 'company-box
    (with-no-warnings
      ;; Highlight `company-common'
      (defun my-company-box--make-line (candidate)
        (-let* (((candidate annotation len-c len-a backend) candidate)
                (color (company-box--get-color backend))
                ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                          (substring (propertize candidate 'face 'company-box-candidate)
                                                     (length company-common) nil)))
                (align-string (when annotation
                                (concat " " (and company-tooltip-align-annotations
                                                 (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                (space company-box--space)
                (icon-p company-box-enable-icon)
                (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                              (company-box--apply-color icon-string i-color)
                              (company-box--apply-color candidate-string c-color)
                              align-string
                              (company-box--apply-color annotation-string a-color)))
                (len (length line)))
          (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                           'company-box--color s-color)
                               line)
          line))
      (advice-add #'company-box--make-line :override #'my-company-box--make-line)

      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

    (when (and (display-graphic-p)
               (bundle-active-p 'icons)
               (require 'all-the-icons nil t))
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
              (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
              (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
              (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
              (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
              (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
              (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.2)))
            company-box-icons-alist 'company-box-icons-all-the-icons)))

  ;; Popup documentation for completion candidates
  (add-hook! 'global-company-mode-hook (company-quickhelp-mode +1))
  (setq company-quickhelp-delay 1)

  ;; company-tabnine
  (setq company-tabnine-binaries-folder
        (lib-f-join (getenv "HOME") ".local/share/tabnine"))
  (defun company-backend-with-tabnine ()
    "Add to `company-backends'"
    (interactive)
    (setq company-backends
          (append (list #'company-tabnine) company-backends)))
  (defun company-backend-remove-tabnine ()
    "delete tabnine `company-tabnine'"
    (interactive)
    (setq company-backends
          (delete #'company-tabnine company-backends))
    (setq company-backends
          (delete '(company-tabnine :with company-yasnippet) company-backends)))

  (with-eval-after-load 'company-tabnine
    (defun tabnine-start-advice (orig &rest rest)
      (with-temp-message
          (with-current-buffer " *Minibuf-0*" (buffer-string))
        (let ((inherit-message t))
          (apply orig rest))))
    (advice-add 'company-tabnine-start-process :around 'tabnine-start-advice)
    ;; The free version of TabNine is good enough,
    ;; and below code is recommended that TabNine not always
    ;; prompt me to purchase a paid version in a large project.
    ;; (defun company-disable-tabnine-upgrade-message (orig args &rest _)
    ;;   (when (and args
    ;;              (stringp (funcall args)))
    ;;     (unless (string-match "The free version of TabNine only indexes up to"
    ;;                           (funcall args))
    ;;       (apply orig args))))
    ;; (advice-add 'company-echo-show :around 'company-disable-tabnine-upgrade-message)
    (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
      (let ((company-message-func (ad-get-arg 0)))
        (when (and company-message-func
                   (stringp (funcall company-message-func)))
          (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
            ad-do-it))))
    ))
