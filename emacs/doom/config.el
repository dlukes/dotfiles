;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq org-directory "~/Desktop/org/")

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
(setq magit-diff-refine-hunk nil)

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
(use-package! key-chord
  :config
  ;; NOTE: Increase if key chords fail to register, decrease if they
  ;; trigger even when you don't mean it or when there's a lag when
  ;; typing normally (!)
  (setq key-chord-one-keys-delay 0.02
        key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
  (key-chord-mode 1))

(use-package! hydra)

(defhydra dlukes/hydra-zen (:timeout 4)
  "Adjust the width of the zen mode writing area"
  ("+" writeroom-increase-width "wider")
  ("-" writeroom-decrease-width "narrower")
  ("0" writeroom-adjust-width "reset")
  ("ESC" nil "done" :exit t))

;; NOTE: Cf. 'SPC h f map\!'. Use stuff like :n immediately before a
;; mapping for Evil state (Vim mode) specific keymaps. Also, glean
;; inspiration from the official key binding definitions, e.g. in:
;;
;; ~/.config/emacs/modules/config/default/+evil-bindings.el
(map! :leader
  :desc "Run ex command" "SPC" #'evil-ex
  :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
  (:prefix ("+" . "hydra") "z" #'dlukes/hydra-zen/body)
  (:prefix ("g" . "git") :desc "Toggle word diff" "w" #'magit-diff-toggle-refine-hunk)
  (:prefix ("w" . "window") "o" #'delete-other-windows)

  ;; NOTE: Workspaces are also easily manipulated with other default key
  ;; bindings:
  ;;
  ;; - Ctrl/Cmd-T         ->  Create new workspace
  ;; - Ctrl/Cmd-Shift-T   ->  Display workspace tab bar
  ;; - Ctrl/Cmd-<number>  ->  Switch to workspace <number>
  :desc "workspace" "W" doom-leader-workspace-map)
