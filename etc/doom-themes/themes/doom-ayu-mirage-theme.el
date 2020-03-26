;;; doom-abyss-theme.el --- inspired by ayu mirage
(require 'doom-themes)

;;
(defgroup doom-ayu-mirage-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ayu-mirage-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-comment-bg doom-ayu-mirage-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-mirage-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-ayu-mirage
  "A dark theme inspired by ayu mirage"

  ;; name        default   256       16
  ((bg         '("#1f2430" nil       nil            ))
   (bg-alt     '("#191e2a" nil       nil            ))
   (base0      '("#101521" "black"   "black"        ))
   (base1      '("#171d27" "#1e1e1e" "brightblack"  ))
   (base2      '("#202834" "#2e2e2e" "brightblack"  ))
   (base3      '("#283240" "#262626" "brightblack"  ))
   (base4      '("#384556" "#3f3f3f" "brightblack"  ))
   (base5      '("#56697f" "#525252" "brightblack"  ))
   (base6      '("#688099" "#6b6b6b" "brightblack"  ))
   (base7      '("#7fa0b2" "#979797" "brightblack"  ))
   (base8      '("#9caab6" "#dfdfdf" "white"        ))
   (fg-alt     '("#dad8d3" "#bfbfbf" "brightwhite"  ))
   (fg         '("#cbccc6" "#2d2d2d" "white"        ))

   (grey       '("#5c6773" nil       nil))
   (red        '("#f28779" "#ff6655" "red"          ))
   (orange     '("#ffa759" "#dd8844" "brightred"    ))
   (green      '("#bae67e" "#99bb66" "green"        ))
   (teal       '("#95e6cb" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ffd580" "#ECBE7B" "yellow"       ))
   (blue       '("#5ccfe6" "#51afef" "brightblue"   ))
   (dark-blue  '("#73d0ff" "#2257A0" "blue"         ))
   (magenta    '("#ffcc66" "#c678dd" "magenta"      ))
   (violet     '("#ffe6b3" "#a9a1e1" "brightmagenta"))
   (cyan       '("#f29e74" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#d48fff" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   (doom-lighten base1 0.1))
   (selection      base5)
   (builtin        red)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.25))
   (constants      dark-cyan)
   (functions      yellow)
   (keywords       orange)
   (methods        yellow)
   (operators      cyan)
   (type           blue)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         "#2a3546")
   (error          "#ff3333")
   (warning        yellow)
   (success        green)
   (vc-modified    "#77abd9")
   (vc-added       "#a6cc70")
   (vc-deleted     "#f27983")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-ayu-mirage-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-mirage-padded-modeline
      (if (integerp doom-ayu-mirage-padded-modeline) doom-ayu-mirage-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-ayu-mirage-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property :foreground blue)
   (css-selector :foreground yellow)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground dark-blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken dark-blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken dark-blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; helm
   (helm-action :foreground fg :underline t)
   (helm-candidate-number :foreground bg :background yellow)
   (helm-header-line-left-margin :foreground bg :background yellow)
   (helm-prefarg :foreground green)
   (helm-separator :foreground red)

   ;; helm buffer
   (helm-buffer-archive :foreground orange)
   (helm-buffer-directory :foreground red :background base4)
   (helm-buffer-not-saved :foreground yellow)
   (helm-buffer-process :foreground cyan)
   (helm-buffer-saved-out :foreground error)
   (helm-buffer-size :foreground dark-cyan)

   ;; helm bookmark
   (helm-bookmark-addressbook :foreground orange)
   (helm-bookmark-file :foreground dark-blue)
   (helm-bookmark-file-not-found :foreground base6)
   (helm-bookmark-gnus :foreground teal)
   (helm-bookmark-info :foreground green)
   (helm-bookmark-man :foreground cyan)
   (helm-bookmark-w3m :foreground yellow)

   ;; helm commad
   (helm-m-x-key :foreground orange :underline t)

   ;; helm elisp
   (helm-lisp-completion-info :foreground red)
   (helm-lisp-show-completion :background base5)

   ;; helm files
   (helm-delete-async-message :foreground yellow)
   (helm-ff-denied :foreground error :background base4)
   (helm-ff-dotted-symlink-directory :foreground orange :background base5)
   (helm-ff-invaild-symlink :foreground error :background base4)
   (helm-ff-pipe :foreground yellow  :background base4)
   (helm-ff-socket :foreground teal)
   (helm-ff-suid :foreground fg :background error)
   (helm-history-remote :foreground red)

   ;; helm locate
   (helm-locate-finish :foreground green)

   ;; helm mode
   (helm-mode-prefix :background red :foreground base0)

   ;; helm occur
   (helm-resume-need-update :background error)

   ;; helm tags
   (helm-etags-file :foreground yellow :underline t)

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background bg)
   (org-block-begin-line :background bg :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;; paren match
   ((paren-face-match &override) :background base5)

   ;; slime
   (slime-early-deprecation-warning-face :strike-through warning)
   (slime-late-deprecation-warning-face :strike-through orange)
   (slime-final-deprecation-warning-face :strike-through red)
   (slime-style-warning-face :underline warning)
   (slime-warning-face :underline orange)
   (slime-error-face :underline error)
   (slime-note-face :underline dark-cyan)
   (slime-repl-inputed-output-face :foreground red)
   (sldb-restartable-frame-line-face :foreground green)

   ;; sly
   (sly-error-face :underline error)
   (sly-note-face :underline dark-cyan)
   (sly-style-warning-face :underline warning)
   (sly-warning-face :underline orange)
   (sly-stickers-armed-face :foreground bg :background blue)
   (sly-stickers-empty-face :foreground bg :background red)
   (sly-stickers-recordings-face :foreground bg :background green)

   ;; dired+
   (diredp-deletion :foreground base0 :background red)
   (diredp-deletion-file-name :foreground red)
   (diredp-flag-mark :foreground base0 :background green)
   (diredp-flag-mark-line :foreground base0 :background yellow)

   ;; ace-window
   (aw-background-face :foreground base6)
   (aw-leading-char-face :foreground red)

   ;; lispy
   (lispy-face-hint :foreground dark-blue :background base0)

   ;; diredp
   (diredp-omit-file-name :strike-through nil)
   (diredp-ignored-file-name :foreground base7)

   ;; info+
   (info-command-ref-item :foreground green :background base4)
   (info-constant-ref-item :foreground red :background base4)
   (info-double-quoted-name :forergound blue)
   (info-file :foreground yellow  :background base4)
   (info-function-ref-item :foreground blue :background base4)
   (info-macro-ref-item :foreground yellow  :background base4)
   (info-menu :foreground yellow)
   (info-quoted-name :foreground green)
   (info-reference-item :background base4)
   (info-single-quote :foreground orange)
   (info-special-form-ref-item :foreground yellow :background base4)
   (info-string :foreground green)
   (info-syntax-class-item :foreground teal :background base4)
   (info-user-option-ref-item :foreground red :background base4)
   (info-variable-ref-item :foreground orange :background base4)

   ;; mingus
   (mingus-album-face :foreground cyan :underline cyan)
   (mingus-album-stale-face :foreground cyan)
   (mingus-artist-face :foreground blue)
   (mingus-current-song-props :foreground base3)
   (mingus-directory-face :foreground yellow)
   (mingus-mark-face :foreground red)
   (mingus-pausing-face :foreground fg-alt)
   (mingus-playing-face :foreground green)
   (mingus-playlist-face :foreground orange)
   (mingus-song-file-face :foreground teal)
   (mingus-stopped-face :foreground vc-deleted)

   ))

;;; doom-ayu-mirage-theme.el ends here
