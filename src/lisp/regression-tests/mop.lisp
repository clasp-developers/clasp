(in-package #:clasp-tests)

;;; TODO: We don't really test MOP behavior here yet.

;;; Test that all the AMOP generic functions are defined
;;; and are generic.
;;; CL symbols are commented out but kept for reference.
(test mop.generics.fboundp
      (let ((mop-generic-names
              '(clos:accessor-method-slot-definition clos:add-dependent
                clos:add-direct-method clos:add-direct-subclass
                ;; clos:add-method clos:allocate-instance
                clos:class-default-initargs clos:class-direct-default-initargs
                clos:class-direct-slots clos:class-direct-subclasses
                clos:class-direct-superclasses clos:class-finalized-p
                #+(or)clos:class-name clos:class-precedence-list
                clos:class-prototype clos:class-slots
                ;; :clos:compute-applicable-methods
                clos:compute-applicable-methods-using-classes
                clos:compute-class-precedence-list
                clos:compute-discriminating-function
                clos:compute-effective-method
                clos:compute-effective-slot-definition
                clos:compute-slots clos:direct-slot-definition-class
                clos:effective-slot-definition-class
                clos:ensure-class-using-class
                clos:ensure-generic-function-using-class
                clos:finalize-inheritance clos:find-method-combination
                clos:generic-function-argument-precedence-order
                clos:generic-function-lambda-list
                clos:generic-function-method-class
                clos:generic-function-method-combination
                clos:generic-function-methods clos:generic-function-name
                ;; make-instance
                clos:make-method-lambda clos:map-dependents
                clos:method-function clos:method-generic-function
                clos:method-lambda-list clos:method-specializers
                clos:method-qualifiers clos:slot-definition-allocation
                clos:slot-definition-initfunction clos:slot-definition-initform
                clos:slot-definition-name clos:slot-definition-type
                clos:slot-definition-readers clos:slot-definition-writers
                clos:slot-definition-location clos:reader-method-class
                clos:remove-dependent clos:remove-direct-method
                clos:remove-direct-subclass ; remove-method
                ;; (setf clos:class-name)
                (setf clos:generic-function-name)
                (setf clos:slot-value-using-class)
                clos:slot-boundp-using-class clos:slot-makunbound-using-class
                clos:slot-value-using-class clos:specializer-direct-generic-functions
                clos:specializer-direct-methods clos:update-dependent
                clos:validate-superclass clos:writer-method-class)))
        (loop for n in mop-generic-names
              always (and (fboundp n)
                          (typep (fdefinition n) 'generic-function)))))

;;; Test that the non-generic functions are available too.
;;; (We don't test that they're not generic, because they're not
;;;  required to not be generic.)
(test mop.nongenerics.fboundp
      (let ((nongeneric-names
              '(clos:eql-specializer-object clos:extract-lambda-list
                clos:extract-specializer-names)))
        (loop for n in nongeneric-names
              always (fboundp n))))

;;; Test that writers for most functions are NOT defined.
;;; This is only very likely for a naively defined accessor, but still.
(test mop.writers.nonfboundp
      (let ((nonwriters
              '(clos:accessor-method-slot-definition clos:add-dependent
                clos:add-direct-method clos:add-direct-subclass
                clos:class-default-initargs clos:class-direct-default-initargs
                clos:class-direct-slots clos:class-direct-subclasses
                clos:class-direct-superclasses clos:class-finalized-p
                clos:class-precedence-list
                clos:class-prototype clos:class-slots
                clos:compute-applicable-methods-using-classes
                clos:compute-class-precedence-list
                clos:compute-discriminating-function
                clos:compute-effective-method
                clos:compute-effective-slot-definition
                clos:compute-slots clos:direct-slot-definition-class
                clos:effective-slot-definition-class
                clos:ensure-class-using-class
                clos:ensure-generic-function-using-class
                clos:finalize-inheritance clos:find-method-combination
                clos:generic-function-argument-precedence-order
                clos:generic-function-lambda-list
                clos:generic-function-method-class
                clos:generic-function-method-combination
                clos:generic-function-methods
                clos:make-method-lambda clos:map-dependents
                clos:method-function clos:method-generic-function
                clos:method-lambda-list clos:method-specializers
                clos:method-qualifiers clos:slot-definition-allocation
                clos:slot-definition-initfunction clos:slot-definition-initform
                clos:slot-definition-name clos:slot-definition-type
                clos:slot-definition-readers clos:slot-definition-writers
                clos:slot-definition-location clos:reader-method-class
                clos:remove-dependent clos:remove-direct-method
                clos:remove-direct-subclass
                clos:slot-boundp-using-class clos:slot-makunbound-using-class
                clos:specializer-direct-generic-functions
                clos:specializer-direct-methods clos:update-dependent
                clos:validate-superclass clos:writer-method-class
                clos:eql-specializer-object clos:extract-lambda-list
                clos:extract-specializer-names)))
        (loop for n in nonwriters
              never (fboundp `(setf ,n)))))
