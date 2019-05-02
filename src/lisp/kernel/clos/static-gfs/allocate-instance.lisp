(in-package #:static-gfs)

#|
Deal with ALLOCATE-INSTANCE with a constant class argument.
Users are unlikely to write this, so it comes pretty much entirely from the expansion
in make-instance.lisp.
We special case when the class is a (funcallable-)standard-class and thus no methods
can be defined.
TODO: Add a cell system like for make-instance. Then it would be nearly as fast for
custom classes. The gains would be much less than from MAKE-INSTANCE, though, since
there's no initarg checking or loops to elide.
|#

(defmacro static-allocate-instance (class &rest initargs)
  (let ((metaclass (class-of class)))
    (cond ((eq metaclass (find-class 'standard-class))
           ;; NOTE: In a general expansion, we'd have to make sure the
           ;; initargs are evaluated. But in the context of the make-instance expansion
           ;; we know none of them have side effects, so it's fine.
           ;; Also, we'd have to watch out for the class being resized.
           ;; But if it is resized that will trigger make-instance recompilation,
           ;; so there's no problem here.
           (let ((size (clos::class-size class)))
             `(core:allocate-new-instance ,class ,size)))
          ((eq metaclass (find-class 'clos:funcallable-standard-class))
           (let ((size (clos::class-size class)))
             `(let ((instance (core:allocate-new-funcallable-instance ,class ,size)))
                (clos:set-funcallable-instance-function
                 instance (clos::uninitialized-funcallable-instance-closure instance))
                instance)))
          (t `(allocate-instance ,class ,@initargs)))))
