(defpackage #:static-gfs
  (:use #:cl)
  (:export #:update-constructors)
  (:export #:invalidate-designated-constructors #:invalidate-class-constructors)
  (:export #:precompile-constructor)
  (:export #:invalidate-class-reinitializers*)
  (:export #:invalidate-changers* #:invalidate-class-changers
           #:invalidate-designated-changers))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:add-implementation-package '#:static-gfs '#:cl))
