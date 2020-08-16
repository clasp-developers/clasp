(defpackage #:static-gfs
  (:use #:cl)
  (:export #:update-constructors)
  (:export #:invalidate-designated-constructors #:invalidate-class-constructors)
  (:export #:precompile-constructor))
