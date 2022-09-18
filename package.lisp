(in-package #:cl-user)

(defpackage #:type-30-display
  (:nicknames :t30d)
  (:local-nicknames (#:a #:alexandria))
  (:use #:clim
        #:clim-lisp
        #:mcclim-render
        #:clime
        #:uiop)
  (:export #:main
           #:type-30-display-entry-point))
