;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! org-roam-ui)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))
(package! ox-odt
  :recipe (:host github :repo "kjambunathan/org-mode-ox-odt" :nonrecursive t
           :files ("lisp/ox-odt.el"
                   "lisp/odt.el"
                   "etc"
                   "docs"
                   "contrib/odt/LibreOffice")))

(package! org-re-reveal-citeproc
  :recipe (:host gitlab :repo "oer/org-re-reveal-citeproc"))

;; Maybe start using Zetteldesk.el for turning Org-roam notes into articles?
;; (package! zetteldesk
;;   :recipe (:host github :repo "Vidianos-Giannitsis/zetteldesk.el"))

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

;; Partial horizontal scrolling of tables (and images? but you'd have to configure that
;; manually) in Org Mode. Not sure if it works with visual-fill-column, and at any rate,
;; it breaks my Doom setup unfortunately (can't resize window).
;; (package! phscroll
;;   :recipe (:host github :repo "misohena/phscroll"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)
(package! org-fancy-priorities :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;(unpin! org)
;(unpin! org-roam)
;; Unless emacs-zmq gets pinned as well, pinning emacs-jupyter can get it out of sync
;; with this core dependency, e.g. https://github.com/nnicandro/emacs-jupyter/issues/433
(unpin! jupyter)
