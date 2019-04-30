(defpackage #:static-gfs
  (:use #:cl)
  (:export #:update-constructors)
  (:export #:invalidate-named-constructors #:invalidate-class-constructors)
  (:export #:precompile-constructor))
