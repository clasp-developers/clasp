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

(test disassemble-5-bclasp
      (let ((cmp:*CLEAVIR-COMPILE-HOOK* nil))
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

(test wots-bformat
 (not (string= "" (with-output-to-string (*standard-output*)(core:bformat t "Foo")))))

(test load-stream.3
      (let ((result-string
             (with-output-to-string
                 (*standard-output*)
               (with-input-from-string
                   (load-stream "(defun foo())")
                 (load load-stream :verbose nil :print nil)))))
        (= 0 (length result-string))))

(test load-stream.3a
      (let ((result-string
             (with-output-to-string
                 (*standard-output*)
               (with-input-from-string
                   (load-stream "(defun foo())")
                 (load load-stream :verbose t :print nil)))))
        (not (= 0 (length result-string)))))

(test load-stream.3b
      (let ((result-string
             (with-output-to-string
                 (*standard-output*)
               (with-input-from-string
                   (load-stream "(defun foo())")
                 (load load-stream :verbose nil :print t)))))
        (not (= 0 (length result-string)))))

(test load-stream.3c
      (let ((result-string
             (with-output-to-string
                 (*standard-output*)
               (with-input-from-string
                   (load-stream "(defun foo())")
                 (load load-stream :verbose t :print t)))))
        (not (= 0 (length result-string)))))

(test-expect-error
 compile-1
 (compile-file "sys:regression-tests;I-do-not-exist.lisp")
 :type core:simple-file-error)

(test compile-2
      (string= ""
               (with-output-to-string (*standard-output*)
                 (compile-file "sys:regression-tests;test-compile-file.lisp" :verbose nil :print nil))))

(test compile-4
      (not (string= ""
               (with-output-to-string (*standard-output*)
                 (compile-file "sys:regression-tests;test-compile-file.lisp" :verbose t :print nil)))))

(test compile-4a
      (not (string= ""
               (with-output-to-string (*standard-output*)
                 (compile-file "sys:regression-tests;test-compile-file.lisp" :verbose nil :print t)))))

(test compile-5
      (not (string= ""
               (with-output-to-string (*standard-output*)
                 (compile-file "sys:regression-tests;test-compile-file.lisp" :print t :verbose t)))))
                 
(defun %%blah (&key foo bar)
  (list foo bar))


(test-expect-error
 key-1
 (%%blah :foo)
 :type PROGRAM-ERROR)

(test 
 key-2
 (%%blah :foo :bar))

(test-expect-error
 key-3
 (%%blah :foo 1 :bar)
 :type PROGRAM-ERROR)

(test cl-symbols-1 (not (fboundp 'cl:reader-error)))
(test cl-symbols-2 (not (fboundp 'cl:variable)))

(test-expect-error
 special-operator-p-1
 (special-operator-p 23)
 :type type-error)

(test-expect-error special-operator-p-2 (funcall 'go 23) :type undefined-function)

(test ast-interpreter-1
      (equal (list most-positive-fixnum most-negative-fixnum)
             (funcall
              (compile nil #'(lambda()
                               (load-time-value (list most-positive-fixnum most-negative-fixnum)))))))

(test ast-interpreter-2
      (equal (list (1+ most-positive-fixnum)(1- most-negative-fixnum))
             (funcall
              (compile nil #'(lambda()
                               (load-time-value (list (1+ most-positive-fixnum)(1- most-negative-fixnum))))))))

(test issue-797
      (functionp
       (compile nil                                                           
                '(lambda()
                  (LOAD-TIME-VALUE
                   (FLET ((%F4 (&OPTIONAL &KEY (KEY1 9911946289546) (KEY2 -19296971321001))
                            3904101166444))
                     -128503000536183044))))))

(defun do-call (local-fun)
  (unwind-protect
       (funcall local-fun)
    (progn
      #+clasp(core:fflush)
      )))

(defun reproduce ()
  (block here
    (flet ((local-fun ()
             (return-from here (values :value))))
      (do-call #'local-fun))))

(test test-issue-948-a
 (not (null (multiple-value-list (reproduce)))))

(test test-issue-948-b
 (let ((lock (mp:make-lock :name "Foo")))
   (= 4
      (block nil
        (mp:with-lock (lock)
          (apply (lambda (id) (return id)) (list 4)))))))
