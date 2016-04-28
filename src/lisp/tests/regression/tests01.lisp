(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package :clasp-tests)

(defmacro test (foo &key description )
  `(unless ,foo
    (format t "The test ~a failed~%" ,foo)
    (when ,description (format t "~a~%" ,description))
    (error "Regression test ~a failed!" ,foo)))


(test (inherits-from-instance (make-cxx-object 'ast-tooling:match-callback))
      :description "A derivable class is a CLOS class that derives from a C++ class.
They are defined in the clbind library using Derivable<Foo>.
They must be seen as inheriting from the Instance_O class.
If they don't then any code that uses them won't work properly.
Check clasp/include/clasp/core/instance.h header file for the 
Instance_O specialization of TaggedCast")
