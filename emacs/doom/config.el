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


(setq
  confirm-kill-emacs nil
  confirm-kill-processes nil

  completion-ignore-case t
  display-line-numbers-type nil
  calendar-week-start-day 1)



;;;; ------------------------------------------------------- Doom-specific configuration {{{ 1


(setq
  ;; M-SPC is intercepted by GNOME
  doom-leader-alt-key "S-SPC"
  doom-localleader-alt-key "S-SPC m"
  ;; See also <https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-do-i-change-the-fonts>
  doom-font "VictorMono Nerd Font-12"
  doom-theme 'doom-palenight)



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

;; Soft-wrapping by default.
(add-hook 'org-mode-hook #'visual-fill-column-mode)

(setq!
  org-directory "~/Desktop/org/"
  org-attach-id-dir (expand-file-name "attach/" org-directory)

  org-indent-indentation-per-level 0
  org-adapt-indentation 'headline-data
  org-indent-mode-turns-off-org-adapt-indentation nil

  org-pretty-entities t
  org-startup-with-inline-images t
  org-display-remote-inline-images 'cache

  ;; Don't create a separate section for footnotes, put them at the end of the section
  ;; they're in.
  org-footnote-section nil
  ;; Citar uses Vertico as its selection engine, and I want selection to be case
  ;; insensitive. Vertico is compatible with Emacs's default completion system, so this
  ;; is covered by completion-ignore-case above.
  citar-bibliography '("~/.cache/zotero/My Library.json")
  org-cite-csl-styles-dir "~/.local/share/csl"

  org-export-with-creator t

  org-latex-reference-command "\\cref{%s}"
  org-latex-tables-booktabs t
  ;; Tweak org-latex-minted-options to customize minted.
  org-latex-listings 'minted
  ;; Minted needs -shell-escape so that it may call pygments.
  org-latex-pdf-process
  '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
  org-latex-packages-alist
  '(("capitalize" "cleveref")
    ("" "booktabs" t)
    ("" "tabularx")
    ("" "minted" t))

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

  org-roam-dailies-directory "daily/"
  org-roam-dailies-capture-templates
  '(("d" "default" entry)
    "* %?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))

;; Why the hell does this setting default to mailcap of all things, instead of xdg-open?
;; Anyway...
(after! org
  (setcdr (assq 'system org-file-apps-gnu) "xdg-open %s"))

;; If this leads to an error, install TeX Live and update Doom so that it notices that
;; you have LaTeX support.
(after! ox-latex
  (add-to-list 'org-latex-classes
    '("scrartcl"
      "\\documentclass{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

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
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Demote errors to warnings."
  (with-demoted-errors
    "Error rendering inline image (parent node ID changed or Org-roam backlink buffer?): %S"
    (base64-decode-string link)))



;;;; -------------------------------------------------------------------- Other packages {{{1


;; NOTE: This is a Custom variable, so set it with setq!, just in case there are any
;; relevant hooks to trigger. As a general rule, when figuring out how to set Custom
;; variables from config.el, try to just lowercase the name from Custom and replace
;; spaces with hyphens. If that doesn't work, go look at the source code for the package
;; that defines the Custom variable and search for the relevant defcustom defining it.
(setq! writeroom-width 40)

;; Word-granularity diffs can be noisy when the algorithm tries too hard in places where
;; it doesn't make sense. Can be toggled with "SPC g w" (see below) if needed.
(setq!
  magit-diff-refine-hunk nil
  magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; lsp-mode options for rust-analyzer are detailed at
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer
(setq!
  lsp-rust-analyzer-server-display-inlay-hints t
  lsp-rust-analyzer-display-chaining-hints t
  lsp-rust-analyzer-display-parameter-hints t)

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


(use-package! key-chord
  :config
  ;; Increase if key chords fail to register, decrease if they trigger even when you
  ;; don't mean it or when there's a lag when typing normally (!)
  (setq key-chord-one-keys-delay 0.02
   key-chord-two-keys-delay 0.1)
  (key-chord-mode 1))
(use-package! key-seq
  :after key-chord
  :config
  (key-seq-define evil-insert-state-map "fd" 'evil-normal-state))

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
