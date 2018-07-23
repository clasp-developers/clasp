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
        (eql :unbound-variable-error key)))

(defclass foo ()((bar :accessor %bar)))

(test cell-3
      (multiple-value-bind
            (key error)
          (handler-case
              (%bar (make-instance 'foo))
            (unbound-slot (e) (values :unbound-slot-error e))
            (error (p) (values :error p)))
        (eql :unbound-slot-error key)))

(test disassemble-2 (progn
                      (disassemble 'car)
                      t))
(test disassemble-3 (progn
                      (disassemble #'car)
                      t))
(test disassemble-4 (progn
                      (disassemble '(lambda(a) a))
                      t))
(test disassemble-5 (progn
                      (disassemble '(lambda()) :type :ir)
                      t))

(defun %foo% (n)(* n n))

(test disassemble-6 (progn
                      (disassemble '%foo%)
                      t))

(test disassemble-7 (progn
                      (disassemble (compile nil '(lambda(n) n)))
                      t))

(defgeneric disassemble-example-fn2 (x y z))
(test disassemble-8 (progn
                      (disassemble 'disassemble-example-fn2)
                      t))

(defgeneric disassemble-example-fn3 (x y z))
(defmethod disassemble-example-fn3 ((x t)(y t)(z t)) (list x y z))

(test disassemble-9 (progn
                      (disassemble ' disassemble-example-fn3)
                      t))

(test load-stream.1
      (with-input-from-string (s "(defun foo())")
        (load s)))

(test load-stream.2
      (with-input-from-string (s "(defun foo())")
        (load s :print t :verbose t)))
