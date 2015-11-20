(setf ccl:*default-external-format* :utf-8)
;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Babel2
; (load "/home/dvl/Documents/dropbox/LISP/Babel2/libraries/asdf")
; (load "/home/dvl/Documents/dropbox/LISP/Babel2/init-babel")
(load (merge-pathnames #p".local/Babel2/libraries/asdf" (user-homedir-pathname)))
(load (merge-pathnames #p".local/Babel2/init-babel" (user-homedir-pathname)))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

