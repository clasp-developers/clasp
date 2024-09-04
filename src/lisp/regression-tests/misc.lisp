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

;;; This tests a problem where the compiler sees that both inputs are
;;; subtypes of FIXNUM (or something) and uses primitive operations;
;;; but one of the inputs is actually bottom (unreachable) and casts
;;; could not deal with it, signaling an error during compilation.
;;; Also causes a failure in the ANSI test misc.110.
(test bottom-primop
      (funcall (compile nil '(lambda (x)
                              (declare (fixnum x))
                              (values
                               (ignore-errors (>= x (the real nil))))))
               0)
      (nil))

(test issue-1382
      (nth-value 2
                 (compile nil
                          '(lambda ()
                            ((lambda (&key x y z) (list x y z))
                             :x 0s0 :y 0s0 :z (coerce (random 100) 'single-float)))))
      (nil))

;;; Hit this and it crashed SLIME.
(test-true positive-bytes-allocated
           (plusp (gctools:bytes-allocated)))

;;; Simple LOOP requires only compound forms. Hence NIL is not
;;; permitted. Some FORMAT directives (like newline) return NIL
;;; as the form when they have nothing to add to the body.
;;; Normally this is fine since BLOCK accepts NIL as a form. On
;;; the other hand, when the newline directive is inside of an
;;; iteration directive this will produce something like
;;; (LOOP (fu) nil (bar)) which is not acceptable. To verify
;;; that this is not happening we make sure we are not getting
;;; (BLOCK NIL NIL) since this is easier to test for.
(test format-no-nil-form
      (third (second (macroexpand-1 '(formatter "~
"))))
      ((block nil)))

;;; Problem caused by incorrect vaslist optimization: These can result in
;;; junk data or segfaults.

(test vaslist-opt-1
      (restart-case (invoke-restart 'bar 1 2 3) (bar (&rest args) (values-list args)))
      (1 2 3))

(test vaslist-opt-2
      (multiple-value-call (lambda (&rest args) (values-list args)) (values-list '(1 2 3)))
      (1 2 3))

;;; Make sure macro-function handles local shadowing correctly
;;; See bug #1556.
(defmacro macro-function-shadowing.f () 3)
(test macro-function-shadowing
      (macrolet ((macro-function-shadowing.f () 1)
                 (e (form &environment env)
                   `',(macroexpand-1 form env)))
        (flet ((macro-function-shadowing.f () 2))
          (declare (ignorable #'macro-function-shadowing.f))
          (e (macro-function-shadowing.f))))
      ((macro-function-shadowing.f)))

;;; This returned junk due to my VM mistake and caused me no end of grief.
(test single-value-catch
      (let ((c (catch 'foo 4))) c)
      (4))
