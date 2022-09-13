;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;; To see tips & recommendations based on the current upstream reference config, open:
;;;
;;;   ~/.config/emacs/core/templates/config.example.el
;;;
;;; Trigger debugger backtraces to investigate problems or just dig deeper into call
;;; stacks you don't quite understand:
;;;
;;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Debugging.html
;;;
;;; tl;dr:
;;;
;;;   debug-on-error t
;;;   debug-on-message "message regexp"



;;;; -------------------------------------------------------------- Personal information {{{1


(setq
  user-full-name "David Lukeš"
  user-mail-address "dafydd.lukes@gmail.com")



;;;; -------------------------------------------------------------- Basic Emacs settings {{{1


(setq!
  ;; Exiting.
  confirm-kill-emacs nil
  confirm-kill-processes nil

  ;; Editing.
  ;; Tweaking this results in a different annoying tradeoff in lisp indentation.
  ;; lisp-indent-offset nil
  completion-ignore-case t

  ;; UI.
  scroll-bar-mode 'right
  display-line-numbers-type nil
  column-number-indicator-zero-based nil
  calendar-week-start-day 1)

(if (not (fboundp #'pixel-scroll-precision-mode))
  ;; then
  (setq mac-mouse-wheel-smooth-scroll t)
  ;; else
  (setq
    pixel-scroll-precision-use-momentum t
    pixel-scroll-precision-large-scroll-height 40.0
    pixel-scroll-precision-interpolation-factor 30)
  (pixel-scroll-precision-mode))



;;;; ------------------------------------------------------- Doom-specific configuration {{{ 1


(setq
  ;; See also <https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-do-i-change-the-fonts>
  doom-font (font-spec :family "BlexMono Nerd Font" :weight 'book)
  doom-theme 'doom-one
  ;; NOTE: GNOME Shell might be stubborn in reverting back to using M-SPC as a keyboard
  ;; layout switching shortcut. If it does, just explicitly switch it to Win-SPC in
  ;; GNOME Tweaks, so that you can use Doom's default (local)leader alt key bindings.
  doom-themes-neotree-file-icons t)



;;;; -------------------------------------------------------------------------- Org Mode {{{1


;; NOTE: Don't try to add Org structure templates with multi-character
;; triggers, it works with C-c C-, but not with org-tempo <-style
;; completion, probably because when there are at least two characters,
;; company completion is triggered instead. Which can probably be
;; tweaked, but at this point, it may be a better idea to use YASnippet
;; instead, < is actually quite inconvenient to type. And twisting my
;; fingers on C-c C-, is of course out of the question.
;; (after! org
;;   (dolist (template '(
;;                        ("ss" . "src shell")
;;                        ("se" . "src emacs-lisp")
;;                        ("sp" . "src jupyter-python")))
;;     (add-to-list 'org-structure-template-alist template)))

;; Soft wrapping with line breaks allowed at more characters than just space or tab.
(setq dlukes/org-category-table (copy-category-table))
(dolist (char '(?- ?+ ?_ ?/ ?| ?\ ?. ?,))
  (modify-category-entry char ?| dlukes/org-category-table))

(add-hook! 'org-mode-hook
  (set-category-table dlukes/org-category-table)
  (setq-local word-wrap-by-category t)
  (visual-fill-column-mode))

;; Right-align Org tags based on whatever EditorConfig sets as the fill-column, but
;; leave room for the three dots that are added in folded views.
(add-hook! 'editorconfig-after-apply-functions
  (setq-local org-tags-column (+ 3 (- fill-column))))

;; Make sure error output via emacs-jupyter has ANSI color sequences fontified, in spite
;; of https://github.com/nnicandro/emacs-jupyter/issues/366.
(defun dlukes/display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'org-babel-after-execute-hook #'dlukes/display-ansi-colors)

;; In Doom Emacs, it's not necessary to configure Babel manually with org-babel-do-load-languages.
;; See https://discourse.doomemacs.org/t/common-config-anti-patterns/.

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; In practice, it often doesn't matter, but I've found this to be true e.g. for
;; org-startup-indented. So let's be on the safe side.
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq!
  org-directory "~/Desktop/org/"
  org-attach-id-dir (expand-file-name "attach/" org-directory)
  org-cite-csl-styles-dir "~/.local/share/zotero/styles")

(after! org
  (setq!
    ;; If you want Org file links to work in exports, you need to use IDs, not the
    ;; default path + text search flavor. This setting automatically generates an ID on
    ;; link creation (if necessary).
    org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
    org-html-self-link-headlines t
    ;; When you store a link while a visual region is selected, the link will contain
    ;; the region as search string after ::.
    org-link-context-for-files t

    ;; Indent mode -- cf. also advice around org-align-tags below.
    ;;
    ;; You *do* want to use org-indent-mode by default, but only to have nice
    ;; indentation in soft-wrapped lists.
    org-startup-indented t
    ;; You don't want additional visual indentation before headings or content.
    org-indent-indentation-per-level 0
    ;; You *do* want additional *physical* indentation for property drawers, clock lines
    ;; and such...
    org-adapt-indentation 'headline-data
    ;; ... in spite of org-indent-mode.
    org-indent-mode-turns-off-org-adapt-indentation nil
    org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))

    org-pretty-entities t
    org-startup-with-inline-images t
    org-display-remote-inline-images 'cache

    ;; The new default is text-properties and it has better performance, but until
    ;; third-party packages (e.g. Org-roam) adapt, it might break fontification, so
    ;; let's stick with overlays for now. TODO: Eventually switch.
    org-fold-core-style 'overlays

    ;; Don't create a separate section for footnotes, put them at the end of the section
    ;; they're in.
    ;; org-footnote-section nil
    ;; Don't turn CSL references into links. This can be useful if you're using
    ;; colorlinks in LaTeX and want the text to be less noisy, or also because links are
    ;; fragile commands and you don't want to have to deal with compilation errors when
    ;; you put references e.g. in captions.
    ;; org-cite-csl-link-cites nil

    ;; Don't export _ and ^ as sub/superscripts unless wrapped in curly brackets. Use
    ;; #+OPTIONS: ^:t (or {} or nil) to tweak on a per-document basis.
    org-export-with-sub-superscripts '{}
    ;; Don't abort export because of broken links, just mark them. Don't enable this by
    ;; default, you probably want to be warned about broken links before possibly
    ;; forcing the export anyway.  Also, ID links can be fixed with
    ;; org-id-update-id-locations or org-roam-update-org-id-locations, so try that
    ;; first.
    ;; org-export-with-broken-links 'mark

    org-latex-tables-booktabs t
    ;; Tweak org-latex-minted-options to customize minted. Minted can also cause
    ;; previewing problems, see org-preview-latex-fragment and
    ;; https://orgmode.org/worg/org-tutorials/org-latex-preview.html.
    org-latex-listings 'minted
    ;; Minted needs -shell-escape so that it may call pygments. Possibly not with
    ;; LuaLaTeX though?
    org-latex-compiler "xelatex"
    org-latex-pdf-process
    '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
    ;; cleveref/cref is nice in theory (it auto-inserts Fig./Tab. etc. based on the type
    ;; of reference), but since it's LaTeX-specific and I might need to export to ODT or
    ;; DOCX too, better not rely on it.
    ;; org-latex-reference-command "\\cref{%s}"
    org-latex-packages-alist
    '(
      ;; ("capitalize" "cleveref")
      ("" "booktabs" t)
      ("" "tabularx")
      ("newfloat" "minted" t)
    )

    ;; Don't prefix figure, table etc. numbers with section numbers.
    org-odt-display-outline-level 0
    org-latex-to-mathml-convert-command "pandoc -f latex -t html5 --mathml %I -o %o"
    ;; This can be used to include *any* LaTeX in ODT exports as PNG images.
    ;; Unfortunately, it can't be used in conjunction with the MathML convert command
    ;; above as it overrides it and equations are also rendered as PNG, which is
    ;; suboptimal.
    ; org-odt-with-latex 'dvipng
    ;; Use this to convert the resulting ODT to a different format and use that as
    ;; the result of the export instead. See org-odt-convert-process(es) for how to
    ;; define the way this conversion should happen. By default, soffice is used, but
    ;; you could conceivably use Pandoc as well.
    ; org-odt-preferred-output-format "docx"

    org-roam-capture-templates
    '(
      ;; Should be same as stock, with different key.
      ("n" "default" plain nil
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
        :unnarrowed t)
      ("t" "tagged" item "- tags :: %?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
        :empty-lines-before 1
        :unnarrowed t)
      ("d" "date" entry "* %^u\n%?"
        ;; There's also a file+datetree target, but that feels unnecessarily verbose --
        ;; hierarchical, always adds an ID.
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
        :empty-lines-before 1
        :unnarrowed t))
    org-roam-dailies-capture-templates
    '(("d" "default" entry "* %U %?"
        :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
        :empty-lines-before 1
        :unnarrowed t)))

  ;; Entity tweaks.
  (dolist
    (item '(
             ;; Make export of asterisks and stars more consistent across backends.
             ("ast" "\\ast" t "&ast;" "*" "*" "*")
             ("lowast" "\\ast" t "&lowast;" "*" "*" "∗")
             ("star" "\\star" t "&star;" "*" "*" "☆")
             ("starf" "\\star" t "&starf;" "*" "*" "★")
             ("sstarf" "\\star" t "&sstarf;" "*" "*" "⋆")
             ()
          ))
    (add-to-list 'org-entities-user item))

  ;; Why the hell does this setting default to mailcap of all things, instead of
  ;; xdg-open?  Anyway...
  (setcdr (assq 'system org-file-apps-gnu) "xdg-open %s")
  (setcdr (assq t org-file-apps-gnu) "xdg-open %s"))

;; One last bit of tweaking for how you want org-indent-mode to work. Turns out tag
;; alignment is broken (or badly configured in my case?) in combination with
;; org-indent-mode: nested headings add indentation to the tag column. You don't want
;; that. TODO: Investigate root cause and possibly report?
(defadvice! dlukes/fix-org-align-tags-under-org-indent-mode (oldfun &rest r)
  "Turn off org-indent-mode when aligning tags, to prevent additional indentation."
  :around 'org-align-tags
  (if (not org-indent-mode)
    (apply oldfun r)
    (org-indent-mode -1)
    (apply oldfun r)
    (org-indent-mode)))

;; If this leads to an error, install TeX Live and update Doom so that it notices that
;; you have LaTeX support. Remember you can control the order of inclusion of (default)
;; packages and extra header lines, and even entirely prevent it. See variable's
;; documentation.
(after! ox-latex
  (dolist
    (item '(
             ;; The intended use of the custom-* classes is that you'll put a
             ;; custom.cls file or symlink in the same dir as the source text, so that
             ;; you can keep the same heading mappings for all classes of the same broad
             ;; kind (article, book, etc.). Basically, custom.cls can simply just
             ;; contain whatever you'd put in the header of your .tex file, except
             ;; instead of \documentclass, it needs to invoke \LoadClass.
             ("custom-article"
               "\\documentclass{custom}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

             ("custom-book"
               "\\documentclass{custom}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]"
               ("\\chapter{%s}" . "\\addchap{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

             ("scrartcl"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

             ("scrbook"
               "\\documentclass{scrbook}"
               ("\\chapter{%s}" . "\\addchap{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ))
  (add-to-list 'org-latex-classes item)))

;; To be able to store links to Emacs Info pages. Enabled by default in vanilla Org, but
;; Doom disables it.
(use-package! ol-info)

(use-package! ox-extra
  :after org
  :config
  ;; Put an :ignore: tag on a headline to only include its subtree contents, not the
  ;; headline itself, in exports. This is useful when you want some headlines to be used
  ;; just for organization or folding purposes, but not reflected in the final document
  ;; structure.
  (ox-extras-activate '(ignore-headlines)))

(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam
  ;; This is just for reference, I can't currently think of a reasonable hook to use.
  ;; This would open ORUI each time the backlinks buffer is toggled -- no.
  ; :hook (org-roam-mode . org-roam-ui-open)
  ;; This would launch the ORUI server when Org Mode is activated -- probably not
  ;; necessary.
  ; :hook (org-mode . org-roam-ui-mode)
  ;; The best thing to do is probably to just have a handy keyboard shortcut to invoke
  ;; ORUI when I want it (see below).
  :config
  (setq!
    org-roam-ui-sync-theme t
    org-roam-ui-follow t
    org-roam-ui-update-on-save t
    org-roam-ui-open-on-start t))

;; When Org-roam tries to render images in the backlinks buffer but can't find them, the
;; filename gets interpreted as a base64 string, which results in an error and rendering
;; of the buffer halts, with the remaining backlinks not shown.
;;
;; Attachments are looked up via the ID of their parent node. So what are the typical
;; reasons why an attachment can't be found?
;;
;; Maybe the image has been refiled under a different ID and the attachment hasn't been
;; moved. Automatic moving of attachments on refile may be implemented in Org-roam in
;; the future, but isn't currently.
;;
;; But more generally, maybe the whole mechanism of attachment lookup fails in the
;; backlinks buffer -- maybe the node ID isn't correctly set in that context. Maybe it's
;; Doom's fault, Doom customizes the attachment system, using a single global dir.
;;
;; At any rate, I don't really care if the backlinks buffer doesn't show images (maybe I
;; even prefer that it doesn't), but I *do* care if it doesn't show all the backlinks
;; due to an error. So demote the error to a warning message. This also means that Org
;; buffers with broken attachment links will still fully initialize, which is also nice.
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data. Demote errors to warnings."
  :override #'+org-inline-image-data-fn
  (with-demoted-errors
    "Error rendering inline image (parent node ID changed or Org-roam backlink buffer?): %S"
    (base64-decode-string link)))



;;;; ----------------------------------------------------------------------------- Citar {{{1


(defadvice! dlukes/citar-file-trust-zotero (oldfun &rest r)
  "Leave Zotero-generated file paths alone, especially zotero://..."
  :around '(citar-file-open citar-file--find-files-in-dirs)
  (cl-letf (((symbol-function 'file-exists-p) #'always)
            ((symbol-function 'expand-file-name) (lambda (first &rest _) first)))
    (apply oldfun r)))

(setq!
  ;; Citar uses Vertico as its selection engine, and I want selection to be case
  ;; insensitive. Vertico is compatible with Emacs's default completion system, so this
  ;; is covered by completion-ignore-case above.
  citar-bibliography '("~/.cache/zotero/My Library.json")
  citar-notes-paths '("~/Desktop/org/roam/reading-notes"))

(after! citar
  (dolist
    (ext '("pdf" "odt" "docx" "doc"))
      (add-to-list 'citar-file-open-functions `(,ext . citar-file-open-external))))



;;;; -------------------------------------------------------------------- Other packages {{{1


(after! embark
  (setq!
    ;; NOTE: This is the default, putting this here mainly to remind myself of embark-act
    ;; (bound to C-; or SPC a) and of the fact that this setting can be toggled per
    ;; invocation by using the C-u universal prefix argument (rebound to M-u in my case
    ;; because of Evil).
    embark-quit-after-action t))

(after! writeroom-mode
  (setq! writeroom-width 40))

(after! magit
  (setq!
    ;; Word-granularity diffs can be noisy when the algorithm tries too hard in places
    ;; where it doesn't make sense. Can be toggled with "SPC g w" (see below) if needed.
    magit-diff-refine-hunk nil
    magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! lsp-mode
  (setq!
    ;; lsp-mode options for rust-analyzer are detailed at
    ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer
    lsp-rust-analyzer-server-display-inlay-hints t
    lsp-rust-analyzer-display-chaining-hints t
    lsp-rust-analyzer-display-parameter-hints t))

;; Spell checking should be available, but not enabled by default, so remove the hooks
;; added in ~/.config/emacs/modules/checkers/spell/config.el
(remove-hook!
  '(org-mode-hook
    markdown-mode-hook
    TeX-mode-hook
    rst-mode-hook
    mu4e-compose-mode-hook
    message-mode-hook
    git-commit-mode-hook)
  #'flyspell-mode)
(remove-hook!
  '(yaml-mode-hook
    conf-mode-hook
    prog-mode-hook)
  #'flyspell-prog-mode)

(after! company
  (setq!
    ;; Company completion can be slow, especially in long-running sessions with lots of
    ;; (Org-roam?) buffers open. This makes typing extremely annoying. So don't trigger
    ;; automatically, use C-SPC to bring it up as required.
    company-idle-delay nil))



;;;; ------------------------------------------------------------------ Custom functions {{{1


(defun dlukes/ediff-doom-config (file)
  "ediff the current config with the examples in doom-emacs-dir

There are multiple config files, so FILE specifies which one to
diff.
"
  (interactive
    (list (read-file-name "Config file to diff: " doom-private-dir)))
  (let* ((stem (file-name-base file))
         (customized-file (format "%s.el" stem))
         (template-file-regex (format "^%s.example.el$" stem)))
    (ediff-files
      (concat doom-private-dir customized-file)
      ;; The templates are in various places unfortunately, so let's do a recursive
      ;; search on the repo, that should work reliably.
      (car
        (directory-files-recursively
          doom-emacs-dir
          template-file-regex
          nil
          ;; The naming of path manipulation in Emacs Lisp is a mess. We want to
          ;; match against the last part of the path, which is what
          ;; file-name-nondirectory is for, but only if the path doesn't end with /,
          ;; because the function is meant for regular files only. So if the last
          ;; portion of the path is a directory, ending in /, you have to convert to
          ;; a "directory file name" (I kid you not, that's the language the docs
          ;; use) with directory-file-name, stripping the / suffix, so that you can
          ;; use file-name-nondirectory on it. However, the paths that are passed to
          ;; our predicate lambda, although exclusively directories, do NOT have the
          ;; / suffix (yay for consistency I guess?), so we can directly call
          ;; file-name-nondirectory to get the last path element.
          (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))



;;;; ----------------------------------------------------------------- Keyboard mappings {{{1


(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(map! :map global-map
  "M-u" #'universal-argument)

(use-package! hydra)
(defhydra dlukes/hydra-zen (:timeout 4)
  "Adjust the width of the zen mode writing area"
  ("+" writeroom-increase-width "wider")
  ("-" writeroom-decrease-width "narrower")
  ("0" writeroom-adjust-width "reset")
  ("q" nil "done" :exit t))

;; NOTE: Cf. 'SPC h f map\!'. Use stuff like :n immediately before a mapping for Evil
;; state (Vim mode) specific keymaps. Also, glean inspiration from the official key
;; binding definitions, e.g. in:
;;
;; ~/.config/emacs/core/core-keybinds.el
;; ~/.config/emacs/modules/config/default/+evil-bindings.el

(map! :leader
  :desc "Run ex command" "SPC" #'evil-ex
  :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
  (:prefix ("+" . "hydra")
    :desc "Adjust zen width" "z" #'dlukes/hydra-zen/body)
  (:prefix ("g" . "git")
    :desc "Toggle word diff" "w" #'magit-diff-toggle-refine-hunk)
  (:prefix ("h" . "help")
    (:prefix ("d" . "doom")
      "D" #'dlukes/ediff-doom-config))
  (:prefix ("n" . "notes")
    (:prefix ("r" . "roam")
      :desc "Show graph" "g" #'org-roam-ui-open))
  (:prefix ("w" . "window")
    "o" #'delete-other-windows)

  ;; Workspaces are also easily manipulated with other default key bindings:
  ;;
  ;; - Ctrl/Cmd-T         ->  Create new workspace
  ;; - Ctrl/Cmd-Shift-T   ->  Display workspace tab bar
  ;; - Ctrl/Cmd-<number>  ->  Switch to workspace <number>
  :desc "workspace" "W" doom-leader-workspace-map)

;; Not necessary since https://github.com/hlissner/doom-emacs/pull/5371 got merged, kept
;; for future reference.
;; (after! org
;;   (map! :map org-mode-map :localleader
;;     "f" #'org-footnote-action))

(map! :map doom-leader-toggle-map
  :desc "Visual fill column" "v" #'visual-fill-column-mode)

;;; vi: foldmethod=marker
