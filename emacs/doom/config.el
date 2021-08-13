;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq confirm-kill-emacs nil
      ;; M-SPC is intercepted by GNOME
      doom-leader-alt-key "S-SPC"
      doom-localleader-alt-key "S-SPC m")

;; NOTE: Turn on to get backtraces when calling interactive commands.
;; (setq debug-on-error t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "David Lukes"
      user-mail-address "dafydd.lukes@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
;; NOTE: See also <https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-do-i-change-the-fonts>
(setq doom-font "VictorMono Nerd Font-12")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/Desktop/org/"
       org-attach-id-dir (expand-file-name "attach/" org-directory)

       org-indent-indentation-per-level 0
       org-adapt-indentation 'headline-data
       org-indent-mode-turns-off-org-adapt-indentation nil

       org-pretty-entities t
       ;; NOTE: Don't create a separate section for footnotes, put them at the end of
       ;; the section they're in.
       org-footnote-section nil
       org-startup-with-inline-images t

       org-roam-dailies-directory "daily/"
       org-roam-dailies-capture-templates
       '(("d" "default" entry
          "* %?"
          :if-new (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n"))))
(add-hook 'org-mode-hook #'visual-fill-column-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; NOTE: This is a Custom variable, so set it with setq!, just in case
;; there are any relevant hooks to trigger. As a general rule, when
;; figuring out how to set Custom variables from config.el, try to just
;; lowercase the name from Custom and replace spaces with hyphens. If
;; that doesn't work, go look at the source code for the package that
;; defines the Custom variable and search for the relevant defcustom
;; defining it.
(setq! writeroom-width 40)

;; NOTE: Word-granularity diffs can be noisy when the algorithm tries
;; too hard in places where it doesn't make sense. Can be toggled with
;; "SPC g w" (see below) if needed.
(after! magit
  (setq magit-diff-refine-hunk nil
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; NOTE: lsp-mode options for rust-analyzer are detailed at
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer
(setq lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints t)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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

(use-package! key-chord
  :config
  ;; NOTE: Increase if key chords fail to register, decrease if they
  ;; trigger even when you don't mean it or when there's a lag when
  ;; typing normally (!)
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
      ;; NOTE: The templates are in various places unfortunately, so
      ;; let's do a recursive search on the repo, that should work
      ;; reliably.
      (car (directory-files-recursively
             doom-emacs-dir
             template-file-regex
             nil
             ;; NOTE: The naming of path manipulation in Emacs Lisp is a
             ;; mess. We want to match against the last part of the
             ;; path, which is what file-name-nondirectory is for, but
             ;; only if the path doesn't end with /, because the
             ;; function is meant for regular files only. So if the last
             ;; portion of the path is a directory, ending in /, you
             ;; have to convert to a "directory file name" (I kid you
             ;; not, that's the language the docs use) with
             ;; directory-file-name, stripping the / suffix, so that you
             ;; can use file-name-nondirectory on it. However, the paths
             ;; that are passed to our predicate lambda, although
             ;; exclusively directories, do NOT have the / suffix (yay
             ;; for consistency I guess?), so we can directly call
             ;; file-name-nondirectory to get the last path element.
             (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))))

;; NOTE: Cf. 'SPC h f map\!'. Use stuff like :n immediately before a
;; mapping for Evil state (Vim mode) specific keymaps. Also, glean
;; inspiration from the official key binding definitions, e.g. in:
;;
;; ~/.config/emacs/core/core-keybinds.el
;; ~/.config/emacs/modules/config/default/+evil-bindings.el
(map! :leader
  :desc "Run ex command" "SPC" #'evil-ex
  :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
  (:prefix ("+" . "hydra") "z" #'dlukes/hydra-zen/body)
  (:prefix ("g" . "git") :desc "Toggle word diff" "w" #'magit-diff-toggle-refine-hunk)
  (:prefix ("h" . "help")
    (:prefix ("d" . "doom")
      "D" #'doom-debug-mode             ; originally under d, let's put it under D...
      "d" #'dlukes/ediff-doom-config))  ; ... and have the config diff take its place
  (:prefix ("w" . "window") "o" #'delete-other-windows)

  ;; NOTE: Workspaces are also easily manipulated with other default key
  ;; bindings:
  ;;
  ;; - Ctrl/Cmd-T         ->  Create new workspace
  ;; - Ctrl/Cmd-Shift-T   ->  Display workspace tab bar
  ;; - Ctrl/Cmd-<number>  ->  Switch to workspace <number>
  :desc "workspace" "W" doom-leader-workspace-map)

(after! org
  (map! :map org-mode-map :localleader
    "f" #'org-footnote-action))
