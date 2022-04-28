(in-package #:clasp-tests)

(test-expect-error
 misc-1
 (disassemble 0)
 :type type-error)

(test-true misc-2
           (> (length (with-output-to-string (*standard-output*)
		        (room)))
              0))

;;; cell-errors

;;; Failed, see void intrinsic_error, now throws ERROR_UNDEFINED_FUNCTION
(test-expect-error cell-1 (eval '(im-am-not-defined)) :type undefined-function)
(test-expect-error cell-2 (eval 'im-am-not-bound) :type unbound-variable)

(defclass foo ()((bar :accessor %bar)))

(test-expect-error cell-3 (%bar (make-instance 'foo)) :type unbound-slot)

(defun test-disassemble (&rest args)
  (let ((*standard-output* (make-broadcast-stream)))
    (apply #'disassemble args)
    t))

(test-true disassemble-2 (test-disassemble 'car))
(test-true disassemble-3 (test-disassemble #'car))
(test-true disassemble-4 (test-disassemble '(lambda (a) a)))
(test-true disassemble-5 (test-disassemble '(lambda ()) :type :ir))

(test-true disassemble-5-bclasp
           (let ((cmp:*CLEAVIR-COMPILE-HOOK* nil))
             (test-disassemble '(lambda ()) :type :ir)))

(defun %foo% (n) (* n n))

(test-true disassemble-6 (test-disassemble '%foo%))
(test-true disassemble-7 (test-disassemble (compile nil '(lambda (n) n))))

(defgeneric disassemble-example-fn2 (x y z))
(test-true disassemble-8 (test-disassemble 'disassemble-example-fn2))

(defgeneric disassemble-example-fn3 (x y z))
(defmethod disassemble-example-fn3 ((x t)(y t)(z t)) (list x y z))

(test-true disassemble-9 (test-disassemble 'disassemble-example-fn3))

(test-true load-stream.1
           (with-input-from-string (s "(defun foo())")
             (load s)))

(test-true load-stream.2
           (with-input-from-string (s "(defun foo())")
             (load s :print t :verbose t)))

(test-true wots-fmt
           (not (string= "" (with-output-to-string (*standard-output*)(core:fmt t "Foo")))))

(test load-stream.3
      (length
       (with-output-to-string
           (*standard-output*)
         (with-input-from-string
             (load-stream "(defun foo())")
           (load load-stream :verbose nil :print nil))))
      (0))

(test-true load-stream.3a
           (let ((result-string
                   (with-output-to-string
                       (*standard-output*)
                     (with-input-from-string
                         (load-stream "(defun foo())")
                       (load load-stream :verbose t :print nil)))))
             (not (= 0 (length result-string)))))

(test-true load-stream.3b
           (let ((result-string
                   (with-output-to-string
                       (*standard-output*)
                     (with-input-from-string
                         (load-stream "(defun foo())")
                       (load load-stream :verbose nil :print t)))))
             (not (= 0 (length result-string)))))

(test-true load-stream.3c
           (let ((result-string
                   (with-output-to-string
                       (*standard-output*)
                     (with-input-from-string
                         (load-stream "(defun foo())")
                       (load load-stream :verbose t :print t)))))
             (not (= 0 (length result-string)))))

(test-expect-error
 compile-1
 (compile-file "sys:src;lisp;regression-tests;I-do-not-exist.lisp")
 :type file-error)

(test compile-2
      (with-output-to-string (*standard-output*)
        (compile-file "sys:src;lisp;regression-tests;test-compile-file.lisp" :verbose nil :print nil))
      (""))

(test-true compile-4
           (not (string= ""
                         (with-output-to-string (*standard-output*)
                           (compile-file "sys:src;lisp;regression-tests;test-compile-file.lisp" :verbose t :print nil)))))

(test-true compile-4a
           (not (string= ""
                         (with-output-to-string (*standard-output*)
                           (compile-file "sys:src;lisp;regression-tests;test-compile-file.lisp" :verbose nil :print t)))))

(test-true compile-5
           (not (string= ""
                         (with-output-to-string (*standard-output*)
                           (compile-file "sys:src;lisp;regression-tests;test-compile-file.lisp" :print t :verbose t)))))
                 
(defun %%blah (&key foo bar)
  (list foo bar))


(test-expect-error key-1 (%%blah :foo) :type PROGRAM-ERROR)

(test key-2 (%%blah :foo :bar) ((:bar nil)))

(test-expect-error key-3 (%%blah :foo 1 :bar) :type PROGRAM-ERROR)

(test-true cl-symbols-1 (not (fboundp 'cl:reader-error)))
(test-true cl-symbols-2 (not (fboundp 'cl:variable)))

(test-expect-error special-operator-p-1 (special-operator-p 23)
                   :type type-error)

(test-expect-error special-operator-p-2 (funcall 'go 23)
                   :type undefined-function)

(test-true ast-interpreter-1
           (equal (list most-positive-fixnum most-negative-fixnum)
                  (funcall
                   (compile nil #'(lambda ()
                                    (load-time-value (list most-positive-fixnum most-negative-fixnum)))))))

(test-true ast-interpreter-2
           (equal (list (1+ most-positive-fixnum)(1- most-negative-fixnum))
                  (funcall
                   (compile nil #'(lambda()
                                    (load-time-value (list (1+ most-positive-fixnum)(1- most-negative-fixnum))))))))

(test-type issue-797
    (compile nil                                                           
             '(lambda()
               (LOAD-TIME-VALUE
                (FLET ((%F4 (&OPTIONAL &KEY (KEY1 9911946289546) (KEY2 -19296971321001))
                         (declare (ignore key1 key2))
                         3904101166444))
                  -128503000536183044))))
    function)

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

(test test-issue-948-a (reproduce) (:value))

(test test-issue-948-b
      (let ((lock (mp:make-lock :name "Foo")))
        (block nil
          (mp:with-lock (lock)
            (apply (lambda (id) (return id)) (list 4)))))
      (4))

(test-type flet-lambda-error
    (lambda ()
      (flet ((foo () 23))
        (lambda (&key (a #'foo))
          a)))
    function)

(test type-inference-error
      (dotimes (x 10 t)
        (funcall (compile nil '(lambda()
                                (let* ((a nil)
                                       (b (first a))
                                       (c (second a))
                                       (d (third a)))
                                  (declare (ignore b c d))
                                  (format t "Ignoring everything~%"))))))
      (t))

(test test-issue-1158
      (block nil
        (tagbody
           (multiple-value-call
               (lambda (x y)
                 (declare (ignore x))
                 (when (zerop y) (go 2)))
             (values 3 0))
         1 (return nil)
         2 (return t)))
      (t))

(test test-issue-1226-a
      (funcall (compile nil '(lambda (c d) (values (values d c c))))
               18 19)
      (19))

(test test-issue-1226-b
      (funcall (compile nil '(lambda (x c d)
                              (if x
                                  (setf c 19)
                                  (setf c 23))
                              (values (values d c c))))
               t 18 19)
      (19))

(test issue-1240
      (funcall (compile nil '(lambda (iter)
                              (loop (multiple-value-bind (node morep)
                                        (funcall iter)
                                      (unless morep (return (values)))
                                      node))))
               (lambda () (values nil nil)))
      ())

(test-true issue-1251
           (and
            (numberp
             (let ((x nil))
               (setq x (- (FLOAT (FLOAT-RADIX 1.0) 1.0)))))
            (numberp
             (let ((x #\a))
               (setq x (- (FLOAT (FLOAT-RADIX 1.0) 1.0)))))))

(test-true issue-1262
           (numberp
            (funcall
             (LAMBDA (A)
               (BLOCK B6 (MULTIPLE-VALUE-PROG1 A (RETURN-FROM B6 0))))
            :boo)))
            
(test issue-1265
      (funcall (compile nil '(lambda (x y)
                              (if x
                                  (sqrt (the single-float y))
                                  (sqrt (the double-float y)))))
               nil 4d0)
      (2d0))
