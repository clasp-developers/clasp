(defpackage #:static-gfs
  (:use #:cl)
  (:export #:update-constructors)
  (:export #:invalidate-designated-constructors #:invalidate-class-constructors)
  (:export #:precompile-constructor)
  (:export #:invalidate-class-reinitializers*)
  (:export #:invalidate-changers* #:invalidate-class-changers
           #:invalidate-designated-changers)
  ;; make-instance compiler macro
  #-building-clasp
  (:implement #:cl))

#+building-clasp
(progn
  (ext:add-implementation-package '("STATIC-GFS") "CL"))
