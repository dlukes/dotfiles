;; Minimal init.el, typically for testing bleeding edge Org Mode in vanilla Emacs. Run
;; `make autoloads` in the Org Mode repo first, then `emacs -Q -l minit.el`.
(add-to-list 'load-path "~/repos/org-mode/lisp")
(require 'org-loaddefs)
(require 'org)
(message "Org version: %s" org-version)
(setq
  ;; org-foo-bar 'baz
  )
