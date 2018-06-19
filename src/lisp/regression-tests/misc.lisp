(in-package #:clasp-tests)

(test-expect-error
 misc-1
 (disassemble 0)
 :type type-error)

(test misc-2
      (> (length (with-output-to-string (*standard-output*)
		   (room))) 0))

;;; cell-errors

;;; Failed, see void intrinsic_error, now throws ERROR_UNDEFINED_FUNCTION
(test cell-1
      (multiple-value-bind
          (key error)
        (handler-case
            (eval '(im-am-not-defined))
          (undefined-function (e) (values :undefined-functions-error e))
          (program-error (p) (values :program-error p)))
        (eql :undefined-functions-error key)))

(test cell-2
      (multiple-value-bind
          (key error)
        (handler-case
            (eval 'im-am-not-bound)
          (unbound-variable (e) (values :unbound-variable-error e))
          (program-error (p) (values :program-error p)))
        (eql  :unbound-variable-error key)))

(defclass foo ()((bar :accessor %bar)))

(test cell-3
      (multiple-value-bind
          (key error)
        (handler-case
            (%bar (make-instance 'foo))
          (unbound-slot (e) (values :unbound-slot-error e))
          (error (p) (values :error p)))
        (eql :unbound-slot-error key)))
