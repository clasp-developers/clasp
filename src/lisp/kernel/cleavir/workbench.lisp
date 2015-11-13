;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Running slime from bclasp+cleavir - don't load inline.lsp or auto-compile
;;;  --- Testing defun-inline-hook

(progn
  (progn ;; Set up everything for building cclasp from bclasp with auto-compile
    (format t "Loading ASDF system~%")
    (time (require :asdf))
    (load "sys:local-asdf-config.lisp")
    (core::cclasp-features)
    (format t "Loading :clasp-cleavir system~%")
    (time (require :clasp-cleavir))
    (print (core:getpid)))
  (print "Done - you are ready to go"))

(apropos "cleavir-compile-file")

(clasp-cleavir::cleavir-compile-file "sys:tests;targs.lsp")

(progn
  (load "sys:kernel;cleavir;auto-compile.lisp")
  (format t "Loading inline.lisp~%")
  (load "sys:kernel;cleavir;inline.lisp")
  (format t "Done loading inline.lisp~%"))

(eval '(defmethod foo () (zzzzz)))
(eval '(defmethod m () (undefined)))

(print clasp-cleavir:*my-env*)

(defgeneric foo (x y))
(macroexpand '(defmethod foo-close-over (x y) (flet ((xxx () (call-next-method))) #'xxx)))
(macroexpand '(defmethod foo-dont-close-over (x y) t))
(trace clos::walk-method-lambda)

(apropos "my-env")
clasp-cleavir:*my-env*

(trace clasp-cleavir::code-walk-for-method-lambda-closure)


(apropos "method-p")
(trace clos::define-complex-method-combination)
(defgeneric foo1 (a b))
(defmethod foo1 ((a integer) b) (+ 1 2))

(clos::method-p (defmethod zzz (a b)))
(trace)
(untrace clos::effective-method-function)
(trace clos::effective-method-function)
(time (progn (gctools:gc-monitor-allocations t) (foo1 1 2) (foo1 1 2) (gctools:gc-monitor-allocations nil)))


(clasp-cleavir::cleavir-compile 'foo '(lambda() (defun effective-method-function (form &optional top-level &aux first)
  (cond ((functionp form)
	 form)
	((method-p form)
	 (method-function form))
	((atom form)
	 (error "Malformed effective method form:~%~A" form))
	((eq (setf first (first form)) 'MAKE-METHOD)
	 (coerce `(lambda (.combined-method-args. *next-methods*)
		    (declare (special .combined-method-args. *next-methods*))
		    ,(second form))
		 'function))
	((eq first 'CALL-METHOD)
	 (combine-method-functions
	  (effective-method-function (second form))
	  (mapcar #'effective-method-function (third form))))
	(top-level
	 (coerce `(lambda (.combined-method-args. no-next-methods)
		    (declare (ignorable no-next-methods))
		    ,form)
		 'function))
	(t
	 (error "Malformed effective method form:~%~A" form))))) :debug t)


(macrolet ((mac () `(defmethod x ()))) (mac))
(setq core:*eval-with-env-hook* #'cclasp-eval)
(eq  core:*eval-with-env-hook* #'cclasp-eval)

(trace core::eval-with-env-default)
(trace clasp-cleavir::cclasp-eval)


(trace clos::compute-effective-method)
(clos::effective-method-function 
(progn
  (trace cmp::irc-make-tagbody-frame)
  (trace cmp::codegen-tagbody)
  (trace cmp::compile-file*)
  (trace cmp::compile-file-t1expr)
  (trace cmp::t1expr)
  (trace clasp-cleavir:cleavir-compile-file)
  (trace clasp-cleavir::cleavir-compile-file-form))
(print "Hello")
(clasp-cleavir:cleavir-compile-file "sys:tests;tgf.lsp")
(load "sys:tests;tgf.fasl")
(time (baz 1))
(+ 48 32)
(getpid)91639

(clasp-cleavir:cleavir-compile 'bar '(lambda () (defmethod foo (a b) (+ a b))) :debug nil)
(clasp-cleavir:cleavir-compile 'bar2 '(lambda () (defun baz (n) (core:trap-execution "a") (gctools:gc-monitor-allocations t) (dotimes (i n) (foo 1 2)) (gctools:gc-monitor-allocations nil))))
(bar)
(bar2)
(time (baz 2))


(foo 1 2)
(time (baz 3))
(time (progn (foo 1 2) (foo 1 2) (foo 1 2)))
(print "Hello")
(trace cmp::irc-make-tagbody-frame)

(macroexpand '(defmethod foo (a b)))


(bar)
(time (progn (+ 1 2) (+ 1 2) (+ 1 2) ))
(- 936 744)

(foo 1 2 3 4 5 6)
(time (progn (foo 1 2 3 4 5 6) (foo 1 2 3 4 5 6) (foo 1 2 3 4 5 6)))

(time (dotimes (i 10000) (foo 1 2 3 4 5 6)))

(defgeneric bar (a)
  (:method (a) (1+ a))
  (:method ((a number)) (call-next-method))
  (:method ((a real)) (call-next-method))
  (:method ((a integer)) (call-next-method)))

(foo 0)
(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p ((instruction cleavir-ir:set-symbol-value-instruction))
  nil)

(let ((clasp-cleavir:*debug-cleavir* t))
  (compile-file "sys:tests;tp.lsp")
  )
(apropos "foo-bar")
(load "sys:tests;tp.fasl")
(boundp '*foo-bar3*)
*foo-bar3*

(trace cleavir-remove-useless-instructions:remove-useless-instructions)
(trace cleavir-ir:delete-instruction)


(clasp-cleavir:cleavir-compile 'yyy '(lambda (z) (car z)) :debug t)
(clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;foundation.lsp")
(trace cleavir-ir:delete-instruction)
(trace cleavir-remove-useless-instructions:remove-useless-instructions)
(trace clasp-cleavir::cleavir-compile-t1expr)
(apropos "delete-instruction")

(apropos "compiled-function")

(declaim (inline bar2))
(defun-inline-hook 'bar2 '(defun bar2 () 1))

(print "Ready")

(apropos "*compiler*")

cleavir-generate-ast:*compiler*


(apropos "do-inline-hook")


;;; Wipe out .cache/common-lisp/*
;;; wipe out .slime/fasl/2015-06-27/*
;;; clasp_boehm_o -f bclasp -f flow -f cclasp-eh
(room)

(clasp-cleavir:cleavir-compile-file "sys:modules;asdf;build;asdf-part.lsp")
(trace cleavir-generate-ast:generate-ast)
(progn
  (progn ;; Set up everything for building cclasp from bclasp with auto-compile
    (format t "Loading ASDF system~%")
    (time (require :asdf))
    (load "sys:local-asdf-config.lisp")
    (pushnew :cleavir *features*)
    (format t "Loading :clasp-cleavir system~%")
    (time (require :clasp-cleavir))
    (format t "Loading inline.lisp~%")
    (load "sys:kernel;cleavir;inline.lisp")
    (print (core:getpid)))
  (load "sys:kernel;cleavir;auto-compile.lisp")
  (print "Done - you are ready to go"))

(print "Hello")

(clasp-cleavir:cleavir-compile-file "sys:tests;tlist.lsp")


(deftype boolean () '(member nil t))

(trace process-declarations)

(defun remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (process-declarations body t)
    (values body doc)))

(clasp-cleavir::cleavir-compile-file #p"sys:tests;tmv.lsp")
(load "sys:tests;tmv.fasl")


(foo)

(multiple-value-bind (body doc) (remove-documentation '("Hello there" '(member nil t))) (list :body body :doc doc))

(multiple-value-call #'list (values 1 2 3))

(trace remove-documentation)


(defmacro deftype2 (name lambda-list &rest body &environment env)
  (format t "body: ~s~%" body)
  (multiple-value-bind (body doc)
      (remove-documentation body)
    (format t "removed docs body: ~s~%" body)
    (setq lambda-list (copy-list lambda-list))
    (dolist (x '(&optional &key))
      (do ((l (rest (member x lambda-list)) (rest l)))
	  ((null l))
	(let ((variable (first l)))
	  (when (and (symbolp variable)
		     (not (member variable lambda-list-keywords)))
	    (cons-setf-car l `(,variable '*))))))
    (multiple-value-bind (decls lambda-body doc)
	(process-declarations body t)
      (if doc (setq doc (list doc)))
      (let ((function `(function 
			#+ecl(LAMBDA-BLOCK ,name ,lambda-list ,@body)
			#+clasp(lambda ,lambda-list 
			 (declare (core:lambda-name ,name) ,@decls) 
			 ,@doc 
			 (block ,name ,@lambda-body))
			)))
	(when (and (null lambda-list) (consp body) (null (rest body)))
	  (let ((form (first body)))
	    (when (constantp form env)
	      (setq function form))))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   ,@(si::expand-set-documentation name 'type doc)
	   (do-deftype ',name '(DEFTYPE ,name ,lambda-list ,@body)
		       ,function))))))

(deftype2 boolean ()
  "A BOOLEAN is an object which is either NIL or T."
  '(member nil t))


(apropos "defun-inline-hook")


(progn ;; Set up everything for building cclasp from bclasp
  (format t "Loading ASDF system~%")
  (time (require :asdf))
  (load "sys:local-asdf-config.lisp")
  (pushnew :cleavir *features*)
  (format t "Loading :clasp-cleavir system~%")
  (time (require :clasp-cleavir))
;;  (format t "Loading inline-prep.lisp~%")
;;  (load "sys:kernel;cleavir;inline-prep.lisp")
  (format t "Loading inline.lisp")
  (load "sys:kernel;cleavir;inline.lisp"))
(load "sys:kernel;cleavir;auto-compile.lisp")

(defun foo (x) (consp x))
(untrace)

(trace cleavir-generate-ast::convert-form)



(progn ;; Set up everything for building cclasp from bclasp
  (format t "Loading ASDF system~%")
  (time (require :asdf))
  (load "sys:local-asdf-config.lisp")
  (pushnew :cleavir *features*)
  (format t "Loading :clasp-cleavir system~%")
  (time (require :clasp-cleavir))
  (format t "Done.~%"))


(defparameter *a* 1)
(clasp-cleavir::ast-form '(lambda () (multiple-value-call #'list (values 1 2 3))))
(clasp-cleavir::hir-form '(lambda () (setq *a* 'foo)))
(clasp-cleavir::hir-form '(lambda () (let ((a 'foo)) (declare (special a)) (print a))))

(clasp-cleavir::hir-form '(lambda () 1))
(clasp-cleavir::hir-form '(lambda ()(block bar (let ((a 'foo)) (declare (special a)) (print a) (return-from bar nil)))))



(clasp-cleavir::cleavir-compile 'foo '(lambda () (multiple-value-bind (x y) (values 1 2) (list x y))))

(foo)



(clasp-cleavir::cleavir-compile-file "sys:tests;tsmall.lsp")
(load "sys:tests;tsmall.fasl")
(foob)
(ext:compiled-function-file #'foob)



(print "Hello")

(compile-file "sys:tests;tfib.lsp")

(getpid)
(error "foo")
(quit)
(setq core::*debug-flow-control* t)
(setq cmp::*low-level-trace-print* t)
(print "Hello")


(clasp-cleavir::cleavir-compile
 'foo
 '(lambda (depth target-depth &optional targets)
   (unless targets (bformat t "---------------------- depth: %s   target-depth: %s\n" depth target-depth))
   (block here
     (if (= depth 0 )
         (throw 'top nil)
         (foo (1- depth) target-depth (push #'(lambda (x) (return-from here x)) targets))))
   (bformat t "Returning from depth: %s\n" depth)))

(clasp-cleavir::cleavir-compile
 'do-foo
 '(lambda (depth)
   (catch 'top
     (foo depth 0))
   (bformat t "Done do-foo\n")))

(do-foo 5)


(setq core::*debug-flow-control* nil)
(foo 6 3)


(elt '(1 2 3 4 5) 2)

(disassemble 'cfibn)

(clasp-cleavir::cleavir-compile
 'cfibn
 '(lambda (reps num &aux (rnum 0) (p1 0) (p2 0) (z 0))
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (declare (type fixnum reps num rnum p1 p2 z))
   (dotimes (r reps)
     (setq p1 1
	   p2 1
	   rnum (- num 2))
     (dotimes (i rnum)
       (setq z (+ p1 p2)
	     p2 p1
	     p1 z)))
   z))

(float (/ 19.3 3.24))

(


(progn
  (clasp-cleavir::cleavir-compile
   'xx
   '(lambda ()
     (format t "Inside returning (should be EXIT-LAMBDA)--> ~s~%"
      (block nil
	(let ((nle (lambda ()
		     (format t "In exit lambda~%")
		     (return-from nil 'exit-lambda))))
	  (unwind-protect
	       (funcall nle)
	    (format t "In protected form~%")))))))
  (xx)
  (clasp-cleavir::cleavir-compile
   'yy
   '(lambda ()
     (block nil
       (let ((nle (lambda ()
		    (format t "In exit lambda~%")
		    (return-from nil 'exit-lambda))))
	 (unwind-protect
	      (funcall nle)
	   (format t "In protected form~%"))))) :debug t)
  (format t "Outside returning (should be EXIT-LAMBDA)--> ~s~%" (yy)))


(setq cmp:*low-level-trace-print* t)
(clasp-cleavir::cleavir-compile-file "sys:tests;tnle.lsp")
(load "sys:tests;tnle.fasl")
(load "sys:tests;tnle.bc")
(let ((x (xx))) (format t "In let returning (should be EXIT-VALUE): ~s~%" x))

(clasp-cleavir::cleavir-compile-file "sys:tests;tnle2.lsp")
(load "sys:tests;tnle2.fasl")
(let ((x (xx))) (format t "In let returning: ~a~%" x))


(clasp-cleavir::cleavir-compile-file "sys:tests;tunwind.lsp")
(load "sys:tests;tunwind.fasl")
(xx)
(print "Hello")

(clasp-cleavir::build-and-draw-ast "/tmp/test.png" '(lambda () (load-time-value (print 10))))

(clasp-cleavir::cleavir-compile 'foo '(lambda () (load-time-value (print 10))) :debug t)

(print "Hello")
(clasp-cleavir::cleavir-compile-file "sys:tests;tmacro.lsp")


(clasp-cleavir::cleavir-compile 'foo '(lambda () (block nil (let ((form (block in (let (*) (return-from in nil)) (return-from nil nil)))) form))) :debug t)
(foo)

(clasp-cleavir::cleavir-compile 'foo '(lambda () (block nil (let ((form (block in (unwind-protect (return-from in)) (return)))) form))) :debug t)

(block nil
  (let ((form (block in
                (let (*)
                  (return-from in))
                (return-from nil nil))))
    form))

(clasp-cleavir::cleavir-compile 'foo '(lambda () (block nil (funcall #'(lambda () (return-from nil nil))) (print "Returned"))) :debug t)



(clasp-cleavir::cleavir-compile
 'foo '(lambda () (block nil (return-from nil 'foo))) :debug t)


(clasp-cleavir::cleavir-compile
 'foo '(lambda ()
        (block nil
          (let ((form
                 (BLOCK main
                   (LET ((CORE::*HANDLER-CLUSTERS* nil))
                     (RETURN-FROM main (FOO))
                     )
                   (RETURN-FROM main
                         (return-from nil nil)
                     )
                   )))
            form))) :debug t)

(macroexpand '(return))




(block nil (let ((form (BLOCK main (LET ((CORE::*HANDLER-CLUSTERS* nil)) (RETURN-FROM main (FOO))) (RETURN-FROM main (return-from nil nil))))) form))


(clasp-cleavir::cleavir-compile 'foo '(lambda () (block nil (let ((form (handler-case (foo) (error () (return))))) form))) :debug t)
(foo)

(clasp-cleavir::cleavir-compile-file "sys:kernel;asdf;build;asdf.lisp" 
              :output-file (compile-file-pathname "sys:modules;asdf;asdf.lisp" 
                                                  :target-backend (default-target-backend)
                                                  )
              :print t)
(default-target-backend)


(compile 'foo '(lambda (x y) (flet ((bar (&optional (x y) (y y)) (format t "bar>>x: ~s  y: ~s~%" x y))) (format t "foo>>x: ~s  y: ~s~%" x y) (bar))))
(boundp '*a*)

(defun foo (y)
  (flet ((bar (&optional (x y) (y y))
           (format t "bar>>  x: ~s  y: ~s~%" x y)))
    (format t "foo>> y: ~s~%" y)
    (bar)))


(foo 1)

(trace cleavir-environment:eval)
(clasp-cleavir::cleavir-compile-file "sys:tests;tevalwhen.lsp")
(apropos "function-name-p")


;;; Stassats has a problem with this code:
(defun foo () (loop for i below 10 collect i))
;;;   No applicable method for CLEAVIR-ENVIRONMENT:MACRO-FUNCTION with arguments of types SYMBOL VALUE-FRAME 

(setq *print-circle* t)
(defparameter *a* #0='(clasp . #0#))
*a*

(cleavir-env:eval '(progn (defmacro zfoo()) (zfoo)) nil nil)

(clasp-cleavir::cleavir-compile 'nil
                                '(defun fibn (reps num &aux rnum p1 p2 z)
                                  (dotimes (r reps)
                                    (setq p1 1
                                          p2 1
                                          rnum (- num 2))
                                    (dotimes (i rnum)
                                      (setq z (+ p1 p2)
                                            p2 p1
                                            p1 z)))
                                  z) :debug nil)

(time (fibn 10000000 78))
(defparameter *reps* 100000000)
(defparameter *num* 78)
(time (fibn *reps* *num*))
(time (core:cxx-fibn *reps* *num*))

(defun fibn (reps num &aux rnum p1 p2 z)
  (dotimes (r reps)
    (setq p1 1
          p2 1
          rnum (- num 2))
    (dotimes (i rnum)
      (setq z (+ p1 p2)
            p2 p1
            p1 z)))
  z)

(time (core:cxx-fibn 10000000 78))


COMPILE
(time (fibn 10000000 78))
real time          : 13.295 secs
run time           : 13.280 secs
GC bytes consed    : 0 bytes
Clasp bytes consed : 520 bytes
LLVM time          : 0.000 secs
LLVM compiles      : 0

COMPILE-FILE
(clasp-cleavir::cleavir-compile-file "sys:tests;tfib.lsp")
(load "sys:tests;tfib.fasl")
(load-bundle "/Users/meister/Development/clasp/src/lisp/tests/tfib.inlined.bc")

(translate-logical-pathname #P"sys:tests;tfib.inlined.bc")
(time (fibn 10000000 78))
real time          : 10.694 secs
run time           : 10.681 secs
GC bytes consed    : 0 bytes
Clasp bytes consed : 520 bytes
LLVM time          : 0.000 secs
LLVM compiles      : 0









(/ (* (/ 10000000 100000) 16) 13.46)






(clasp-cleavir::cleavir-compile-file "sys:tests;tadd.lsp")
(getpid)3290

(load "sys:tests;tadd.fasl")

(clasp-cleavir::cleavir-compile 't+ '(lambda (x y) (test-two-arg-+ x y)))
(clasp-cleavir::cleavir-compile 't< '(lambda (x y) (test-two-arg-< x y)))
(clasp-cleavir::cleavir-compile 'foo< '(lambda (x y) (t< x y)))
(disassemble 'foo<)

(tadd 1 2)
(+ most-positive-fixnum)4611686018427387903
(+ most-negative-fixnum)-4611686018427387904

(tadd most-positive-fixnum 1) --> Convert this value: -4611686018427387904 to a bignum
NIL

(+ -4611686018427387904 (expt 2 63) )4611686018427387904
(+ most-positive-fixnum 0)4611686018427387903



(tadd most-negative-fixnum -1)Convert this value: 4611686018427387903 to a bignum

(- 4611686018427387903 (expt 2 63)) --> -4611686018427387905
(+ -4611686018427387904 (expt 2 63) )4611686018427387904
(+ most-negative-fixnum 0)-4611686018427387904

(expt 2 4)








*features*


(clasp-cleavir:cleavir-compile nil '(progn (defmacro #1=#.(gensym)()) (#1#)) :debug t)

(macroexpand '(and form1)) -> (LET ((#:G1665 FORM1)) (IF #:G1665 #:G1665 NIL))

(apropos "backtrace")
(trace (cleavir-environment:eval :print ((core:ihs-backtrace))))
(trace cleavir-environment:eval)
(trace clasp-cleavir::cleavir-compile-file-form)
(trace cleavir-generate-ast:generate-ast)
(trace cleavir-generate-ast:convert)
(untrace)

(clasp-cleavir::cleavir-compile-file "sys:tests;tmacro-6times.lsp")

(clasp-cleavir::cleavir-compile 'foo-+ '(lambda (x y) (core:test-two-arg-+ x y)) :debug t)

(apropos "test-two-arg-+")



(untrace)
(trace cleavir-generate-ast:convert-special)
(trace cleavir-generate-ast::convert-form)

(time (foo 1000))


(pushnew :cleavir *features*)
(cc::compile-clasp :init :cclasp :force-recompile nil)
(cc::link :init :cclasp :system clasp-cleavir::*clasp-cleavir-files*)

*clasp-cleavir-files*

(load "sys:kernel;cleavir;cmpclasp.lisp")



(progn
  (format t "Loading clasp-cleavir~%")
  (require :clasp-cleavir))


(progn
  (setq *echo-repl-tpl-read* t)
  (setq *echo-repl-read* t)
  (setq *load-print* t)
  (setq *print-source-code-cons* t))



(disassemble 'cleavir-primop:rplaca)
(clasp-cleavir:cleavir-compile 'test-consp '(lambda () (consp (cons 1 2))))
(clasp-cleavir:cleavir-compile 'test-car '(lambda () (car (cons 1 2))))
(clasp-cleavir:cleavir-compile 'test-cdr '(lambda () (cdr (cons 1 2))))

(clasp-cleavir:cleavir-compile 'test-rplaca '(lambda (x y) (rplaca x y)))
(clasp-cleavir:cleavir-compile 'test-rplacd '(lambda (x y) (rplacd x y)))
(test-consp)
(test-cdr)
(defparameter *a* (cons 1 2))
(test-rplaca *a* 99)
(test-rplacd *a* 100)
(setq *load-print* t)
(setq *load-verbose* t)

(load "sys:kernel;cleavir;cmpclasp.lisp")


(print "Hello")
(disassemble 'test-car)


(ast-form '(lambda (x) (cleavir-primop:car x)))
(hir-form '(lambda (x) (cleavir-primop:car x)))

(ast-form '(lambda (x y) (cleavir-primop:rplaca x y)))
(hir-form '(lambda (x y) (cleavir-primop:rplaca x y)))
(mir-form '(lambda (x y) (cleavir-primop:rplaca x y)))
(mir-form '(lambda (x y) (cleavir-primop:rplacd x y)))

(clasp-cleavir::cleavir-compile 'unsafe-car '(lambda (x) (cleavir-primop:car x)))
(clasp-cleavir::cleavir-compile 'unsafe-cdr '(lambda (x) (cleavir-primop:cdr x)))
(clasp-cleavir::cleavir-compile 'do-rplaca '(lambda (x y) (clasp-cleavir::my-rplaca x y)))
(clasp-cleavir::cleavir-compile 'unsafe-cdr '(lambda (x) (cleavir-primop:cdr x)))

(clasp-cleavir:cleavir-compile 'do-rplaca '(lambda (x y) (clasp-cleavir::my-rplaca x y)))
(disassemble 'my-rplaca)


(clasp-cleavir:cleavir-compile 'do-car '(lambda (x) (car x)))

(do-car (cons 2 2))
(disassemble 'do-car)



(defparameter *a* (cons 1 2))

(do-rplaca *a* 10)
(print *a*)
(disassemble 'do-rplaca)


(clasp-cleavir::cleavir-compile-file #P"sys:tests;tinline.lsp")

(print "Hello")

(in-package :clasp-cleavir)

lambda-list-keywords
(trace cleavir-env:function-info
       cleavir-generate-ast::function-info
       cleavir-environment::make-info
       cleavir-environment::defining-function-info)

(setf *print-escape* t)
(trace cleavir-environment::make-info cleavir-environment::function-inline)
(clasp-cleavir::cleavir-compile 'test-consp '(lambda (x) (if (my-consp x) t nil)) :debug nil)

(draw-ast (cleavir-generate-ast:generate-ast '(lambda (x) (cleavir-primop:car x)) *clasp-env* *clasp-system*))

(clasp-cleavir:cleavir-compile-file #P"sys:tests;tinline.lsp")

(load #P"sys:tests;tinline.fasl")

(foo-consp 1)

(getpid)98985



(defparameter *m* (mir-form '(lambda (x) (declare (core:lambda-name inline-me)) (if (my-consp x) t nil))))
(cc-mir::assign-mir-instruction-datum-ids *m*)
(clasp-cleavir:finalize-unwind-and-landing-pad-instructions *m*)


(draw-ast (global-function-inline-ast 'my-consp) "/tmp/tconsp.png")



(cleavir-env:function-info *clasp-env* 'my-consp)

(trace cleavir-generate-ast:convert-code
       cleavir-generate-ast::convert-special
       cleavir-env:function-info)
(ast-form '(lambda (x) (if (cleavir-primop:consp x) t nil)))
(hoisted-ast-form '(lambda (x) (if (cleavir-primop:consp x) t nil)))
(hir-form '(lambda (x) (if (cleavir-primop:consp x) t nil)))
(mir-form '(lambda (x) (if (cleavir-primop:consp x) t nil)))

(trace cleavir-env:variable-info)
(clasp-cleavir::cleavir-compile 'my-consp '(lambda (x) (if (cleavir-primop:consp x) t nil)) :debug t)

(my-consp 'foo)

(
(constantp 'nil)

(cleavir-compile-file #P"sys:tests;tadd.lsp")

(setq cmp:*low-level-trace-print* t)
(apropos "low-level-trace")

(load #P"sys:tests;tadd.bc")

(print "Hello")

(probe-file #P"sys:..;tests;lisp;tadd.bc")
(fdefinition 'add)

(add 1 2)

(find-symbol (string '#:+fn-prototype-argument-names+) :cmp)


(print "Done")
(foo)

(clasp-cleavir:cleavir-compile 'baz '(lambda () (with-simple-restart (geton "geton restart") (error "testing"))))
(baz)

(cmp:bclasp-compile 'bcatcher '(lambda (f) (declare (core:lambda-name bcatcherl))(catch 'zot (funcall f))))
(cmp:bclasp-compile 'bthrower '(lambda () (throw 'zot 'baz)))
(compile 'cthrower '(lambda () (throw 'zot 'baz)))
(compile 'ccatcher '(lambda (f) (declare (core:lambda-name ccatcher-lambda)) (catch 'zot (funcall f))))
;;                     Using throw-ast    using throwFunction
(bcatcher #'bthrower) ;; --> works           works
(bcatcher #'cthrower) ;; --> works           fails
(ccatcher #'bthrower) ;; --> fails           fails
(ccatcher #'cthrower) ;; --> fails           fails



(clasp-cleavir:cleavir-compile 'foo '(lambda () (let ((*x* 3)) 
						  (progv '(*x*) '(4) 
						    (list *x* (symbol-value '*x*))))))
(foo)


(clasp-cleavir:cleavir-compile 'foo '(lambda () (block foo (funcall #'(lambda () (declare (core:lambda-name inner)) (return-from foo (values 1 2 3)))) (print "Skip")) (print "Done")) :debug t)

(foo)

(clasp-cleavir:cleavir-compile 'a '(lambda () (catch 'foo (b))))
(clasp-cleavir:cleavir-compile 'b '(lambda () (c)))
(clasp-cleavir:cleavir-compile 'c '(lambda () (d)))
(clasp-cleavir:cleavir-compile 'd '(lambda () (declare (core:lambda-name thrower)) (throw 'foo 'bar)))
(getpid)

(catch 'foo (d))

(llvm-sys:dump cmp:*the-module*)

(apropos "table")

(catch 'foo (d))
(a)

(print clasp-cleavir:*debug-cleavir*)
(core:load-time-values-symbols-dump "<compile>")

(setq cmp:*low-level-trace-print* t)

(progn
  'foo
  'bbar
  'cbar)


(let ((cmp:*dump-module-on-completion* t)
      (cmp:*cleavir-compile-hook* #'clasp-cleavir:my-cleavir-compile-t1expr))
  (compile 'badthrow (list "TOP-LEVEL" t "/Users/meister/Development/clasp/src/tests/lisp/bad.bc" )))

(let ((cmp:*dump-module-on-completion* t)
      (cmp:*cleavir-compile-hook* #'clasp-cleavir:my-cleavir-compile-t1expr))
  (compile 'badthrow (list "TOP-LEVEL" t "/Users/meister/Development/clasp/src/tests/lisp/bad01.bc" )))

(getpid)
(fdefinition 'badthrow)#<COMMON-LISP:COMPILED-FUNCTION CORE::UNNAMED-LAMBDA :address 0x11c778080>

(let ((cmp:*dump-module-on-completion* t)
      (cmp:*cleavir-compile-hook* #'clasp-cleavir:my-cleavir-compile-t1expr))
  (compile 'badthrow (list "cl->TOP-LEVEL" t "/Users/meister/Development/clasp/src/tests/lisp/bad03.bc" )))

(let ((cmp:*dump-module-on-completion* t)
      (cmp:*cleavir-compile-hook* #'clasp-cleavir:my-cleavir-compile-t1expr))
  (compile 'badthrow (list "cl->TOP-LEVEL" t "/Users/meister/Development/clasp/src/tests/lisp/bad04.bc" )))

(fdefinition 'badthrow)
(getpid)


(catch 'foo (badthrow))


(let ((cmp:*dump-module-on-completion* t)
      (cmp:*cleavir-compile-hook* #'clasp-cleavir:my-cleavir-compile-t1expr))
  (compile 'goodthrow (list "COMMON-LISP:LAMBDA" nil "/Users/meister/Development/clasp/src/tests/lisp/good01.bc" )))

(defun zzz ()
  (badthrow))

(defun yyy ()
  (zzz))

(catch 'foo (yyy))

(goodthrow)

(fdefinition 'clasp-cleavir:my-cleavir-compile-t1expr)

(in-package :clasp-cleavir)
(defun my-cleavir-compile-t1expr (name info env pathname)
  (format t "In my-cleavir-compile-t1expr~%")
  (let ((cleavir-generate-ast:*compiler* 'cl:compile)
	(main-name (first info))
	(run-setup (second info))
	(bitcode-filename (third info)))
    (format t "In B~%")
    (multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
	;; The Load the module from the file 
	(let ((module (llvm-sys:parse-bitcode-file bitcode-filename cmp:*llvm-context*)))
	  (format t "Loaded bitcode module: ~a~%" module)
	  (setq cmp:*the-module* module))
      (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname))
    (format t "In C~%")
      (when cmp:*dump-module-on-completion* (llvm-sys:dump cmp:*the-module*))
      (if (not cmp:*run-time-execution-engine*)
	  (setq cmp:*run-time-execution-engine* (cmp:create-run-time-execution-engine cmp:*the-module*))
	  (llvm-sys:add-module cmp:*run-time-execution-engine* cmp:*the-module*))
      (setq cmp:*the-module* nil)
      (let* ((fn-name main-name)
	     (fn (llvm-sys:find-function-named cmp:*run-time-execution-engine* fn-name)))
	(or fn (error "Could not find fn ~a" fn-name))
	(let ((setup-function
	       (llvm-sys:finalize-engine-and-register-with-gc-and-get-compiled-function
		cmp:*run-time-execution-engine*
		'REPL			; main fn name
		fn			; llvm-fn
		nil			; environment
		cmp:*run-time-literals-external-name*
		"repl-fn.txt"
		0
		0
		nil)))
	  (unless (compiled-function-p setup-function)
	    (format t "Whoah cleavir-clasp compiled code eval --> ~s~%" compiled-function)
	    (return-from my-cleavir-compile-t1expr (values nil t)))
	  (if run-setup
	      (let ((enclosed-function (funcall setup-function cmp:*run-time-literal-holder*)))
		(format t "Ran the setup function~%")
		(cmp:set-associated-funcs enclosed-function cmp:*all-functions-for-one-compile*)
		(values enclosed-function warnp failp))
	      setup-function
	      ))))))


(lambda () (tagbody (funcall #'(lambda () (go b))) (print "skip") b (print "Done"))) :debug t)

(foo)

(llvm-sys:dump cmp:*the-module*)

(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;teh.lsp")
(load "sys:..;tests;lisp;teh.fasl")
(foo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile parts of clasp
;;;

;; Compile everything
(load "sys:kernel;cleavir-system.lsp")
(clasp-cleavir:compile-clasp :init :cleavir-clasp :system *cleavir-system* :dont-link t)
(cc:link :init :clasp-cleavir  :system clasp-cleavir:*cleavir-system*)


;; Compile auto-compile only
(load "sys:kernel;cleavir-system.lsp")
(clasp-cleavir:compile-clasp :pre-auto-compile :cleavir-clasp :system *cleavir-system* :dont-link t)
(cc:link :init :clasp-cleavir  :system clasp-cleavir:*cleavir-system*)
(print "Done linking")

(macroexpand '(cmp:irc-low-level-trace))
(trace cmp:irc-low-level-trace)

(symbol-package '|kernel/contrib/sicl/Code/Cleavir/Generate-AST/utilities|)
(cc:link :init :clasp-cleavir  :system clasp-cleavir:*cleavir-clasp-all*)

(print "Here Done")



(trace CLEAVIR-GENERATE-AST::CONVERT
       CLEAVIR-GENERATE-AST::CONVERT-FORM
       cleavir-generate-ast::function-info
       cleavir-env:function-info
       cleavir-env:variable-info)


(defun try-cf ()
  (trace CLEAVIR-GENERATE-AST::CONVERT
	 CLEAVIR-GENERATE-AST::CONVERT-FORM
	 cleavir-generate-ast::function-info
	 cleavir-env:function-info
	 cleavir-env:variable-info)
  (clasp-cleavir:cleavir-compile-file "sys:.. ;tests;lisp;tfun.lsp")
  (untrace))

(try-cf)

(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tgo.lsp")
(apropos "function-info")




(with-open-file (clasp-cleavir::*debug-log* "/tmp/rest/rest.log" :direction :output)
  (let ((clasp-cleavir::*debug-log-on* t))
    (clasp-cleavir:compile-clasp 'core:kernel/cmp/compilefile :auto-cleavir :system *cleavir-system* :dont-link t)))



(compile 'foo '(lambda () (tagbody (funcall #'(lambda () (go a))) (print "skip") a (print "done"))))

(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;cf.lsp")


(compile-file "sys:..;tests;lisp;tgo.lsp")
(load "sys:..;tests;lisp;tgo.bc")
(foo 0 5)

(clasp-cleavir:cleavir-compile 'foo '(lambda () (declare (core:lambda-name outer)) (tagbody (funcall #'(lambda () (declare (core:lambda-name inner)) (go a))) (print "skip") a (print "done"))) :debug t )
(foo)

(load "sys:kernel;cleavir;cmpclasp.lisp")

(print "Hello")

(load "sys:kernel;cleavir-system.lsp")

core:*all-cxx-classes*


(print "Hello")

(clasp-cleavir:compile-clasp 'core:kernel/lsp/assert 'core:kernel/cleavir/auto-compile :system *cleavir-system* :dont-link t)


(clasp-cleavir:compile-clasp :temp :cleavir-clasp :system *cleavir-system* :dont-link t)
(cc:link :init :all  :system clasp-cleavir:*cleavir-clasp-all*)
*cleavir-clasp-all*

(symbol-package (find-symbol "TOP-LEVEL"))


(with-open-file (clasp-cleavir::*debug-log* "/tmp/tgo/tgo.log" :direction :output)
  (let ((clasp-cleavir::*debug-log-on* t))
    #+(or)(trace cleavir-generate-ast:convert-special)
    (clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tgo.lsp")
    #+(or)(untrace cleavir-generate-ast:convert-special)
    ))

(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tgo.lsp")
(compile-file "sys:..;tests;lisp;tgo.lsp")

(load "sys:..;tests;lisp;tgo.bc")
(foo 0 5)

(clasp-cleavir:save-all-files)
(load "sys:kernel;cleavir-system.lsp")
(apropos "translate")

(load "sys:..;..;..;..;temp;t.fasl")



(compile-file "sys:..;tests;lisp;ds.lsp" :output-file "sys:..;tests;lisp;bds.fasl")

(load "sys:..;tests;lisp;ds1.fasl")
(find-class 'foo2)



(print "Hello")
(clasp-cleavir:cleavir-compile 'foo '(lambda () (multiple-value-bind (a b) (values 1 2) (values a b))) :debug t)
(foo)

(disassemble 'foo)

(clasp-cleavir:compile-clasp 'core:kernel/cleavir/auto-compile 'core:kernel/cleavir/auto-compile)








(find-class 'foo2)

(core:load-time-values-dump "DS1")
(load-time-values-dump-values "DS1")

(print *x*)

(find-class 'foo2)
(load-time-value (find-class 'foo2))
(make-foo2)

(find-class 'foo1)
(make-foo1)
core:*pi*
(funcall core:*pi*)



(flet ((foo () (tagbody (print "A") AGAIN (return-from foo nil) (print "B")))) (foo))) :debug t)

(funcall (tret))
(print "Hello")


(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tm.lsp")

(load "sys:..;tests;lisp;tm.fasl")

(baz)

(time (clasp-cleavir:compile-clasp 'core:kernel/clos/conditions 'core:kernel/clos/conditions))
(cc:link :init :all)
(print "Done")


(clasp-cleavir:cleavir-compile 
 'packit 
 '(lambda (packages options maybe-list)
   (block packages-iterator
     (let ((all-symbols nil))
       (when (or (atom packages) (not maybe-list))
	 (setq packages (list packages)))
       (dolist (p packages)
	 (let ((package (si::coerce-to-package p)))
	   (multiple-value-bind (hash-ext hash-int packages-used)
	       (si::package-hash-tables package)
	     (when (member :external options)
	       (push (list package :external hash-ext) all-symbols))
	     (when (member :internal options)
	       (push (list package :internal hash-int) all-symbols))
	     (when (member :inherited options)
	       (dolist (p packages-used)
		 (push (list package :inherited (si::package-hash-tables p))
		       all-symbols))))))
       (unless all-symbols
	 (return-from packages-iterator #'(lambda () (values nil nil nil nil))))
       (let* ((current (pop all-symbols))
	      (package (first current))
	      (type (second current))
	      (iterator (si::hash-table-iterator (third current))))
	 (flet ((iterate ()
		  (tagbody
		     (format t "iterate top all-symbols: ~a~%" all-symbols)
		   AGAIN
		     (multiple-value-bind (found key value)
			 (funcall iterator)
		       (declare (ignore key))
		       (format t "found: ~a  value: ~a~%" found value)
		       (cond 
			 (found
			  (when (eq type :inherited)
			    (multiple-value-bind (s access)
				(find-symbol (symbol-name value) package)
			      (unless (and (eq s value) (eq access type))
				(go AGAIN))))
			  (format t "About to return~%")
			  (return-from iterate (values t value type package))
			  (format t "Return never happened~%")
			  (go DONE))
			 ((null all-symbols)
			  (format t "null all-symbols~%")
			  (return-from iterate (values nil nil nil nil)))
			 (t
			  (setq current (pop all-symbols))
			  (format t "Popped all-symbols: ~a~%" all-symbols)
			  (setq package (first current)
				type (second current)
				iterator (si::hash-table-iterator (third current))
				))))
		     (go AGAIN)
		   DONE
		     (format t "Leaving through DONE~%"))))
	   #'iterate))))))

(defvar *pi* (packit :gray '(:external) t))

(funcall *pi*)
(print "Hello")


(time (clasp-cleavir:compile-clasp :init :cleavir-clasp))


(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tclosure.lsp")
(load "sys:..;tests;lisp;tclosure.fasl")




(apropos "load-time-value")

(define-symbol-macro foo bar)
(macroexpand 'foo)


(apropos "lookup-symbol-macro")
(core:lookup-symbol-macro 'foo nil)
(clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tml.lsp")


(with-open-file (clasp-cleavir::*debug-log* "/tmp/boot/boot.log" :direction :output)
  (let ((clasp-cleavir::*debug-log-on* t))
    (clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tml.lsp")))
(trace cleavir-environment:macro-function)
(untrace cl:macroexpand-1)


(compile-file "sys:..;tests;lisp;tml.lsp")
(load "sys:..;tests;lisp;tmacro.bc")

(defparameter *v* #(0 0 0 0))
(write0 *v* 1)


(time (clasp-cleavir:compile-cleavir-clasp))

(with-open-file (clasp-cleavir::*debug-log* "/tmp/boot/boot.log" :direction :output)
  (let ((clasp-cleavir::*debug-log-on* t))
    (time (clasp-cleavir:compile-clasp-with-cleavir 'core::kernel/clos/boot 'core::kernel/clos/boot :force-recompile t :reload nil :system clasp-cleavir::*cleavir-clasp-all*))
    ))

;;
;; Compile from :MIN to :CLEAVIR-CLASP
;;

(trace cleavir-environment:symbol-macro-expansion)

(time (clasp-cleavir:compile-clasp-with-cleavir 'core::kernel/lsp/assert :cleavir-clasp :force-recompile t :reload nil :system clasp-cleavir::*cleavir-clasp-all*))

(print "Hello")


(defun bitcode-pathname (module &key (target-backend (core::default-target-backend)))
  (merge-pathnames (pathname (string module)) 
		   (make-pathname :host target-backend :directory '(:absolute) :type "bc")))


(defun select-bitcode-files (start end &key (target-backend (core::default-target-backend))
					 (system core::*system-files*))
  (let ((rest (member start system)))
    (loop for mod in (member start system)
       until (eq mod end)
       unless (keywordp mod)
       collect (bitcode-pathname mod :target-backend target-backend))))


(defun link-clasp (start end &key (target-backend "CLEAVIR-BOEHM"))
  (let ((bitcode-files (select-bitcode-files start end :target-backend target-backend)))
    (cmp:link-system-lto (core::target-backend-pathname core::+image-pathname+ 
							:target-backend target-backend)
			 :lisp-bitcode-files bitcode-files
			 :prologue-form '(progn
					  (if (member :interactive *features*) 
					      (core:bformat t "Starting %s Clasp %s ... loading image... it takes a few seconds\n" (if (member :use-mps *features*) "MPS" "Boehm" ) (software-version))))
			 :epilogue-form '(progn
					  (cl:in-package :cl-user)
					  (core::process-command-line-load-eval-sequence)
					  (when (member :interactive *features*) (core:run-repl)))
			 :target-backend target-backend)
    ))


(link-clasp :init :cmp :target-backend "CLEAVIR-BOEHM")
(print "Done")

(apropos "with-early-accessors")




(select-bitcode-files :init :cmp)

(print "Hello")


(locate 'c '(a b c d e f))


(core:lisp-source-pathname 'core:kernel/lsp/foundation)


(print "Hello")





(with-open-file (fout "sys:kernel;all-files.lsp" :direction :output)
  (print clasp-cleavir::*cleavir-clasp-all* fout))


(defparameter *files* (let* ((fin (open "sys:kernel;all-files.lsp" :direction :input)) (files (read fin))) (close fin) (remove 'core::cmprepl files) ))


(compile-clasp-with-cleavir 'core:clos/conditions :all)



(load "sys:kernel;cleavir;cleavir-files.lisp")

(defvar *cleavir-clasp-only* (lisp-executable.creation:determine-complete-set-of-asdf-source-files (list :clasp-cleavir)))

(defvar *cleavir-clasp-all* (append core:*init-files* *cleavir-clasp-only* (list :cleavir-clasp)))

(first *cleavir-clasp-only*)|kernel/contrib/sicl/Code/Cleavir/Input-output/packages|

*cleavir-clasp-all*
(core::lisp-source-pathname '|kernel/contrib/sicl/Code/Cleavir/Input-output/packages|)

(clasp-cleavir:compile-system :all :cleavir-clasp :system *cleavir-clasp-all*)


(append (remove 'core:cmp/cmprepl core:*init-files*) (lisp-executable.creation:determine-complete-set-of-asdf-source-files (list :clasp-cleavir)) (list :cleavir-clasp)))

(clasp-cleavir:compile-system :all :cleavir-clasp :system *cleavir-clasp*)




*files*

(pathname-directory (first *files*))
(pathname-directory (translate-logical-pathname (make-pathname :host "SYS")))
(enough-namestring (first *files*) (translate-logical-pathname (make-pathname :host "SYS")))
(translate-logical-pathname (make-pathname :host "sys"))



(destructuring-bind (a b . c) (list 1 2 3 4 5 6)
  (print c))
(apropos "builtin")

(clos:classp (find-class 'fixnum))
(apropos "classp")


(trace cleavir-env:function-info)
(untrace)
(fdefinition 'clos:slot-definition-name)

(apropos "slot-definition-slots")


(symbol-macrolet ((x 'foo)) (list x (let ((x 'bar)) x)))
(let () (symbol-macrolet ((x 'foo)) (list x (let ((x 'baz)) x))))

(apropos "compile-clasp")

(apropos "image-pathname")


(print "Hi there")
(trace sys::get-sysprop)


(constantp 'clos::+the-standard-class+)

*features*





(clasp-cleavir:cleavir-compile 'foo 
			       '(lambda ()
				 (MULTIPLE-VALUE-CALL
				     #'(LAMBDA (&OPTIONAL (X) (Y) (Z) &REST #:G14358)
					 (PRINT (LIST X Y Z))
					 (VALUES X Y Z))
				   (core:FUNWIND-PROTECT
				    (LAMBDA () (MULTIPLE-VALUE-CALL
						   #'(LAMBDA (&OPTIONAL (A) (B) (C) &REST #:G14359) (print (list a b c))(VALUES A B C))
						 (VALUES 1 2 3)))
				    (LAMBDA () (PRINT "unwind-block"))))) :debug nil)



(clasp-cleavir:cleavir-compile 'foo 
			       '(lambda ()
				 (MULTIPLE-VALUE-CALL
				     #'(LAMBDA (&OPTIONAL (X) (Y) (Z) &REST #:G14358)
					 (PRINT (LIST X Y Z))
					 (VALUES X Y Z))
				   (core:FUNWIND-PROTECT
				    #'(lambda () (print "protected") (values 1 2 3))
				    #'(lambda () (print "unwind"))))) :debug t)




(foo)

(with-open-file (clasp-cleavir:*debug-log* "/tmp/values/tvalues.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir:cleavir-compile-file "sys:..;tests;lisp;tvalues.lsp")
    ;;    (common-lisp-user:compile-clasp-with-cleavir 'core:clos/hierarchy 'core:clos/hierarchy :recompile t)
    ))



(macroexpand '(multiple-value-bind (fn function-kind wrapped-env lambda-name warnp failp)
	       (with-debug-info-generator (:module *the-module* 
					   :pathname pathname)
		 (multiple-value-bind (llvm-function-from-lambda lambda-name)
		     (compile-lambda-function definition env)
		   (or llvm-function-from-lambda (error "There was no function returned by compile-lambda-function inner: ~a" llvm-function-from-lambda))
		   (core:bformat t "Got function from compile-lambda-function: %s\n" llvm-function-from-lambda)
		   (core:values-testing llvm-function-from-lambda :function env lambda-name)))
	       fn))






(foo)

(load "sys:..;tests;lisp;tpush.bc")
(defvar *a* nil)
(push 'a *a*)


(apropos "*primitives*")
cmp::*primitives*2



(cleavir-compile 'foo 
		 '(lambda () 
		   (let ((z (multiple-value-bind (x y)
				(multiple-value-bind (a b)
				    (values 9 10)
				  (values 1 2)
				  (values 3 4)
				  (values a b))
			      (values 10 20)
			      (values x y))))
		     (or z (warn "z is nil")))))
(foo)

(cleavir-ir:map-instructions (lambda (i) (format t "~a~%" (cc-mir:describe-mir i))) *hir*)

(cc-mir:describe-mir *hir* t)


(setf (clasp-cleavir:instruction-gid *hir*) 10)
(clasp-cleavir:instruction-gid *hir*)

(defvar *a*)
(defvar *b*)
(asdf:operate 'asdf:monolithic-concatenate-source-op :clasp-cleavir :build-pathname "cleavir-all")
(print "Hello")
*b*
(asdf:output-files *a*)



(with-open-file (clasp-cleavir:*debug-log* "/tmp/ttest.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;ttest.lsp")
    ))


(with-open-file (clasp-cleavir:*debug-log* "/tmp/tblock/tblock.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tblock.lsp")
    ))

(cleavir-compile 'foo '(lambda () (labels ((bar (x) (print "in bar") (return-from bar) )) (bar 1))) :debug t)

(foo)
(print "Hello")


(in-package :clasp-cleavir)
(cleavir-compile 'foo '(lambda (x y) (+ x y 1)) :debug t)


(foo 1 2)

(trace cleavir-ir-graphviz:draw-datum)
(load "sys:kernel;cleavir;gml-drawing.lisp")

(cleavir-ir-gml:draw-flowchart *hir* "/tmp/!mir.gml")
(print "Hello")


(fdefinition 'cleavir-ir-graphviz:draw-datum)

(with-open-file (clasp-cleavir:*debug-log* "/tmp/tc/tc.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tc.lsp")
    ))
(print "Hello")

clasp-cleavir:*entry-irbuilder*

(llvm-sys:dump cmp:*the-module*)

clasp-cleavir:*vars*



(with-open-file (clasp-cleavir:*debug-log* "/tmp/tl.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tl.lsp")
    ))

(load "sys:..;tests;lisp;tl.fasl")
(foo 1 2)
(core:getpid)60684

(define-symbol-macro foo 9999)


(defmacro check-symbol-macro (sym &environment env)
  (format t "symbol-macro-expansion for: ~a --> ~a~%" sym (macroexpand sym env))
  nil)

;;;   xxxxxx
(clasp-cleavir:cleavir-compile 'foo '(lambda () (symbol-macrolet ((bar 1)) (check-symbol-macro foo) (check-symbol-macro bar) (print bar))))

(defparameter *a* 1)
(clasp-cleavir:cleavir-compile 'foo '(lambda (x)
				      (let ((*a* 2))
					(format t "inner *a* = ~a~%" *a*))
				      (format t "outer *a*=~a~%" *a*)) :debug t)

(with-open-file (clasp-cleavir:*debug-log* "/tmp/tsp/tsp.log" :direction :output)
  (let ((*compile-print* t))
    (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tsp.lsp")
    ))

(load "sys:..;tests;lisp;tsp.fasl")
(foo)

(let ((*compile-print* t))
  (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tsp.lsp")
  )






(let ((*debug-log* t))
  (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;dm.lsp"))
(load "sys:..;tests;lisp;dm.bc")
(test)

(core:getpid)58962

(clasp-cleavir:cleavir-compile 'mtest '(core:fset 'test #'(lambda (x y) (+ x y 1) )) )


(mtest 1 2)
(test)
(cleavir


 (cleavir-compile 'foo '(lambda () #'(lambda () (labels ((foo () (print "foo"))) (foo)))) :debug t)


 (cleavir-compile 'foo '(lambda () (core::multiple-value-call #'list (values 1 2))) :debug t)

 (cleavir-compile 'foo '(lambda () (defun traverse-car (tree)
				     (labels ((tcar (subtree)
						(when subtree (tcar (car subtree)))))
				       (tcar tree))))
		  :debug t)

 (cleavir-compile 'foo '(lambda () (labels ((fun () (fun))) (fun))) :debug t)
 (cleavir-compile 'foo '(labels ((fun1 () (fun2)) (fun2 () (fun2))) (fun1)) :debug t)



 (load "sys:..;tests;cleavir;basic.lsp")


 (foo)

 (cleavir-compile 'foo '(lambda (x y) (declare (core:lambda-name foo-lambda)) (declare (ignore x)) (+ x y)) :debug t )


 (cadr (find 'b '((a 1) (b 2)) :key #'car))

 (let ((*debug-cleavir* t))
   (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;testltv.lsp"))

 (core:getpid)


 +temp-i8+
 (cleavir-compile 'foo '(lambda (ht)
			 (loop :for name :being :the :hash-keys :of ht :do
			    (print ht))))
 (defparameter h (make-hash-table :test #'eq))
 (dolist (l '(a b c d e))
   (setf (gethash l h) l))
 (foo h)

 (macroexpand '(defconstant +i8+ (llvm-sys:type-get-int8-ty *llvm-context*)))
 (PROGN
   (*MAKE-CONSTANT '+I8+ (LLVM-SYS:TYPE-GET-INT8-TY *LLVM-CONTEXT*))
   NIL
   (EVAL-WHEN (:COMPILE-TOPLEVEL)
     (*MAKE-CONSTANT '+I8+ (LLVM-SYS:TYPE-GET-INT8-TY *LLVM-CONTEXT*))
     (CORE::REGISTER-GLOBAL '+I8+))
   '+I8+)





 (require :asdf)
 (asdf:load-system :clasp-cleavir)

 (macro-function 'asdf:load-system)

 (core:getpid)
 (print "Hello")


 (cleavir-compile 'mv0 '(lambda () (block hello (ff (lambda (x) (return-from hello (gg x)))))) :debug t)

 (cleavir-compile 'mvpathological '(lambda () (multiple-value-call #'list (block foo (funcall (lambda () (return-from foo (values 1 2))))) (block bar (funcall (lambda () (return-from bar (values 3 4))))))) :debug t)




 (core:multiple-value-funcall #'list (lambda () (block foo (funcall (lambda () (return-from foo (values 1 2)))))) (lambda () (block bar (funcall (lambda () (return-from bar (values 3 4)))))))

 (cleavir-compile 't0 '(lambda () 0) :debug t)


 (progn
   (push :arguments cmp:*low-level-trace*)
   (push :all cmp:*low-level-trace*)
   (setq cmp:*low-level-trace-print* t))

 (compile-file "sys:..;tests;lisp;tiny3.lsp")
 (load "sys:..;tests;lisp;tiny3.fasl")
 (foo 1 2)

 (clasp-cleavir::cleavir-compile 'tmv '(lambda () (multiple-value-call #'list (values 1 2) (values 3 4) (values 5 6 7 8 9) )) :debug t)

 (tmv)
 (print clasp-cleavir:*debug-cleavir*)
 (draw-ast *ast*)

 (make-package "CLASP-CLEAVIR")
 (cmp:load-bitcode "sys:..;tests;lisp;tiny0.bc")
 (ta 7 :y 1 :y 2 :y 3 :y 4 :z 5)

 *var*
 (setq cmp:*debug-compile-file* t)
 (with-open-file (clasp-cleavir:*debug-log* "/tmp/compilefile.log" :direction :output)
   (let ((cmp:*compile-print* t))
     (clasp-cleavir::cleavir-compile-file "sys:kernel;cmp;compilefile.lsp")
     ))

 (load "sys:kernel;lsp;foundation.fasl")
 (print "Hello")

 (clasp-cleavir::cleavir-compile-file "sys:kernel;asdf;build;asdf.lisp")

 (apropos "max-character-type-index")



 (let ((clasp-cleavir:*debug-cleavir* t))
   (clasp-cleavir::cleavir-compile-file "sys:..;tests;lisp;tiny1.lsp"))
 (draw-ast *ast*)



 (load "sys:..;tests;lisp;tiny1.fasl")





 (let ((clasp-cleavir:*debug-cleavir* t))
   (cleavir-compile 't0 
		    '(lambda (x &key (y nil y-p) (z nil z-p)) 
		      (list x (list y y-p) (list z z-p)))))
 (t0 1 :y 2)

 (cleavir-compile 't0 '(lambda (item &optional (list #'eq)) 1))
					;&key key (test2 1) test-not) ))




 (cleavir-compile 'ta '(lambda (x &optional (y 888) (z 999)) (list x y z)))

 (t0 7 :y 2)


 (ta 7)

 (let ((times 1000000)
       (code '(lambda (x mult) 
	       (let ((total 0) 
		     (count 0))
		 (tagbody 
		  top
		    (setq total (+ total x))
		    (setq count (1+ count))
		    (if (< count mult)
			(go top)))
		 total))))
   (compile 'clasp-test code)
   (cleavir-compile 'cleavir-clasp-test code)
   (format t "cleavir-clasp-test~%")
   (time (format t "cleavir-clasp result: ~a~%" (cleavir-clasp-test 1 times)))
   (format t "clasp-test~%")
   (time (format t "        clasp result: ~a~%" (clasp-test 1 times))))

 (fdefinition 'clasp-test-defun)


 (compile 'adotimes '(lambda (x n) (let ((total 0)) (dotimes (i n) (setq total (+ total x))) total)))

 (compile 'ahand '(lambda (x mult) 
		   (let ((total 0) 
			 (count 0))
		     (tagbody 
		      top
			(setq total (+ total x))
			(setq count (1+ count))
			(if (< count mult)
			    (go top))
			)
		     total)))

 (cleavir-compile 'cdotimes '(lambda (x n) (let ((total 0)) (dotimes (i n) (setq total (+ total x))) total)))

 (cleavir-compile 'chand '(lambda (x mult) 
			   (let ((total 0) 
				 (count 0))
			     (tagbody 
			      top
				(setq total (+ total x))
				(setq count (1+ count))
				(if (< count mult)
				    (go top))
				)
			     total)))

 (time (adotimes 1 1000000))
 (time (ahand    1 1000000))
 (time (cdotimes 1 1000000))
 (time (chand    1 1000000))


 (lambda (x n) (let ((total 0)) (dotimes (i n) (setq total (+ total x))) total)))

(disassemble 'tdo)




(let ((code '(lambda (x n) (let ((total 0)) (dotimes (i n) (setq total (+ total x))) total)))
      (num 1000000))
  (cleavir-compile 'cleavir-clasp-dotimes code)
  (compile 'clasp-dotimes code)
  (time (format t "cleavir-clasp result: ~a~%" (cleavir-clasp-dotimes 1 num)))
  (time (format t "        clasp result: ~a~%" (clasp-dotimes 1 num))))


;;;; My hand written loop
(defun clasp-test (x mult) 
  (let ((total 0) 
	(count 0))
    (tagbody 
     top
       (setq total (+ total x))
       (setq count (1+ count))
       (if (< count mult)
	   (go top))
       )
    total))

;;; A series of macroexpands that expand DOTIMES
(macroexpand '(dotimes (i n) (setq total (+ total x))))
(BLOCK NIL
  (LET* ((CORE::%DOTIMES-VAR N) (I 0))
    (DECLARE)
    (CORE::WHILE (< I CORE::%DOTIMES-VAR)
                 (SETQ TOTAL (+ TOTAL X))
                 (SETQ I (1+ I)))))

(macroexpand '(CORE::WHILE (< I CORE::%DOTIMES-VAR)
	       (SETQ TOTAL (+ TOTAL X))
	       (SETQ I (1+ I))))
(TAGBODY
   (GO #:G2230)
   #:G2229
   (SETQ TOTAL (+ TOTAL X))
   (SETQ I (1+ I))
   #:G2230
   (WHEN (< I CORE::%DOTIMES-VAR) (GO #:G2229)))
T

(macroexpand '(WHEN (< I CORE::%DOTIMES-VAR) (GO #:G2229)))
(IF (< I CORE::%DOTIMES-VAR) (PROGN (GO #:G2229)))
T

(time (tdo-cleavir 1 10000000))
(time   (tdo-clasp 1 10000000))





(defun b (x mult) 
  (let ((total 0) 
	(count 0))
    (tagbody 
     top
       (setq total (+ total x))
       (setq count (1+ count))
       (if (< count mult)
	   (go top))
       )
    total))






(probe-file "sys:..;tests;lisp;tiny1.lsp")
(defun cleavir-primop:call-with-variable-bound (&rest args)
  (error "What do I do now???"))

(setf (fdefinition 'cleavir-primop:call-with-variable-bound) (fdefinition 'core:call-with-variable-bound))

(defparameter *a* 1)
(defun foo () (print (list "*a*" *a*)))
(cleavir-compile 'tspec '(lambda (*a*) (print *a*)#|| (foo) (values 1 2 3)||#))
(tspec 10)


(cleavir-compile 'tspec2 '(lambda () (print 1) (foo)))
(tspec2 1)
*a*



(cleavir-compile 'tup '(lambda () (unwind-protect (progn (print "protected") (values 1 2 3)) (print "cleanup") (values 4 5 6))))

(progn
  (defun foo (fn) (funcall fn))
  (cleavir-compile 'tgo '(lambda () (tagbody a (foo (lambda () (go b))) (print "a") b (print "b")))))
(tgo)

(progn
  (defun diddly (fn) (funcall fn))
  (cleavir-compile 'tret '(lambda () (print (block foo (diddly (lambda () (return-from foo 'bar))) 2)))))
(tret)



clasp-cleavir:*tags*
clasp-cleavir:*basic-blocks*

(loop with x = nil
   do (print "Hello"))

(trace clasp-cleavir:compute-landing-pads)
(core:getpid)581
(tup)


clasp-cleavir-ast-to-hir:*landing-pad*

(tagbody (ff (lambda () (go a)) a))


(tagbody (print "t-a") (core:funwind-protect (lambda () (print "p-a") (go foo) (print "p-b")) (lambda () (print "c-a"))) (print "t-b") foo (print "t-c"))
"t-a" 
"p-a" 
"c-a" 
"t-c"


(hoisted-hir-form '(lambda () (tagbody a (print "a") b (print "b") c (function (lambda () (go a))))))

(hoisted-hir-form '(lambda () (progn (print "Hello") (block a #'(lambda () (print "inner")(return-from a))))))
(apropos "cleanup-ast")
(apropos "enter-instruction")


(in-package :clasp-cleavir)

(trace cmp::codegen-rtv/all)
(cleavir-compile 't2 '(lambda (x) (* x 2)))
(core:load-time-values-dump-symbols "<compile>" 523)
(t2 16) -> 32

(cleavir-compile 'hyp '(lambda (x y) (sqrt (+ (* x x) (* y y)))))
(hyp 2 3) --> 3.60555

(cleavir-compile 'cmp '(lambda (x y) (if (eq x y) "same" "different")))
(cmp 'a 'a) 

(cleavir-compile 'cloop '(lambda (x) (dotimes (i x) (print i))))
(cloop 10)

(cleavir-compile 'uwpr '(lambda (x) (unwind-protect (print "A") (print "B"))))

(defvar *a* 1)

(ast-form '(lambda (x) (let ((*a* 2)) (format t "inner *a* = ~a~%" *a*)) (format t "outer *a*=~a~%" *a*)))

(trace cleavir-generate-ast:convert-special-binding)

(cleavir-compile 'spectest '(lambda (x) (let ((*a* 2)) (format t "inner *a* = ~a~%" *a*)) (format t "outer *a*=~a~%" *a*)))
(llvm-sys:cxx-data-structures-info)

(core:low-level-backtrace)
(apropos "ihs-")
(core::ihs-env 73)

(a 1)


(cleavir-compile 'a '(lambda (x y) (let ((res (+ x y))) res)))
(cleavir-compile 'a '(lambda () (multiple-value-bind () nil)))

(a) -->  42324823482938492834982343234234

(core:load-time-values-dump-values "<default>" 1853)
(print cmp::*run-time-literal-holder*)
(apropos "run-time")
(core:load-time-values-ids)

(ast-form '(lambda () (unwind-protect (print "protected") (print "cleanup1"))))
(hoisted-ast-form '(lambda (x) (+ 1 (- 123123434182312310 x))))
(hoisted-mir-form '(lambda (x) (+ 1 x)))
(hoisted-hir-form '(lambda (x) #'(lambda (y) (+ x y 1))))
(trace (setf cleavir-ir:predecessors))



(compile 'a '1)

(apropos "run-time-literal")
cmp::*run-time-literals-external-name*
(core:load-time-values-dump "globalRunTime")



(print "Hello")

(with-output-to-string (s) (loop for c across "\"abcdef\"" do (if (eql c #\") (princ "\"" s) (princ c s))))
(with-output-to-string (s) (loop for c across "\"abcdef\"" do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s)))

(constantp "this is")


*debug-basic-blocks*

(node-predecessors *hir*)


(print *hir*)
(typep *hir* 'cleavir-ir:enter-instruction)
*hir*

(apropos "enter-instruction")


(class-of *hir*)
(print *debug-basic-blocks*)

(cleavir-basic-blocks:basic-blocks *hir*)


(defun node-predecessors (top)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (node)
	       (when (null (gethash node table))
		 (setf (gethash node table) t)
		 (format t "node: ~a   predecessors: ~a~%" node (cleavir-ir:predecessors node))
		 (let ((succs (cleavir-ir:successors node)))
		   (if (typep node 'cleavir-ir:unwind-instruction)
		       (traverse (first succs))
		       (loop for successor in (cleavir-ir:successors node)
			  do (traverse successor)))
		   (when (typep node 'cleavir-ir:enclose-instruction)
		     (traverse (cleavir-ir:code node)))))))
      (traverse top))))



(defun node-with-write-cell-predecessors (top)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (node)
	       (when (null (gethash node table))
		 (setf (gethash node table) t)
		 (format t "node: ~a   predecessors: ~a~%" node (cleavir-ir:predecessors node))
		 (let ((succs (cleavir-ir:successors node)))
		   (if (typep node 'cleavir-ir:unwind-instruction)
		       (traverse (first succs))
		       (loop for successor in (cleavir-ir:successors node)
			  do (traverse successor)))
		   (when (typep node 'cleavir-ir:enclose-instruction)
		     (traverse (cleavir-ir:code node)))))))
      (traverse top))))


(node-predecessors *hir*)


(typep *hir* 'cleavir-ir:enter-instruction)

(apropos "debug-basic-blocks")

clasp-cleavir:*basic-blocks*

(core:debug-hash-table t)
(core:getpid)

(defclass foo () ())
(defparameter v1 (make-instance 'foo))
(defparameter v2 (make-instance 'foo))
(defparameter ht (make-hash-table :test #'equal))
(setf (gethash (cons v1 v2) ht) '1-2)
(setf (gethash (cons v1 v1) ht) '1-1)
(setf (gethash (cons v2 v1) ht) '2-1)
(setf (gethash (cons v2 v2) ht) '2-2)
(progn (core:debug-hash-table t)(prog1 (gethash (cons v2 v1) ht) (core:debug-hash-table nil))) ; --> |2-1|

(gethash (cons v1 v1) ht) ; --> |1-1|



ht
ht

(untrace gethash)

core:*assert-failure-test-form*


(format t "~S" (every (lambda (x) )))
(core::assert-failure '(every (lambda (x))))
(apropos "assert-failure")
(apropos "*print-")

(untrace)
(trace cl:round)
(trace core::posn-column)
(trace gray::stream-write-char)
(trace core::pretty-out)
(trace core:pretty-stream-buffer-fill-pointer)
(trace gray:stream-force-output)
(setq *print-pretty* nil)
(untrace cl:write-string)
(trace core:output-partial-line)
(trace core:pretty-stream-queue-tail)
(trace core:pretty-stream-queue-head)

(untrace)
(trace core:maybe-output)
(trace core:output-line)
(trace write-string)
(trace gray:stream-write-string)
(trace core:pretty-stream-buffer)
(trace (setf core:pretty-stream-buffer))
(trace core:pretty-sout)
(trace schar-set)
(apropos "schar")
(trace replace)
(trace core:copy-subarray)

(untrace)
(with-open-file (*error-output* "/tmp/error.log" :direction :output)
  (let ((*print-pretty* t)
	(*print-circle* nil)
	(*trace-output* *error-output*))
    (format t "123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.~%")
    (format t "123456789.123456789.1234567")
    (prin1
     '(si::fset 'ext:register-with-pde
       (function 
	(lambda (whole env)
	 (declare (core:lambda-name other-stuff)))
	)
       t)
     )))

(replace #1="abcdef" #1# :start1 2 :start2 0)


(replace "abcdefghijkl" "abcdefghijkl" :start1 0 :start2 3 :end2 12)

(REPLACE "(FSET 'EXT:REGISTER-WITH-PDE #'(LAMBDA (WHOLE ENV) (DECLARE                                                                     " "(FSET 'EXT:REGISTER-WITH-PDE #'(LAMBDA (WHOLE ENV) (DECLARE                                                                     " :START1 33 :START2 29 :END2 60)
(pprint '(cons function))


(find-class 'cleavir-ir:top-level-enter-instruction)#<COMMON-LISP:STANDARD-CLASS CLEAVIR-IR:TOP-LEVEL-ENTER-INSTRUCTION 0x10db804c8>


(core:getpid)
(print "Hello")

(in-package :clasp-cleavir)

(cleavir-compile nil '(lambda (x) (+ x x)))


(llvm-sys:dump cmp::*the-module*)


cmp::*dbg-current-file*
cmp::*dbg-generate-dwarf*

(apropos "cleavir-compile")

(apropos "cleavir")


(apropos "internal-linkage")

(core:low-level-backtrace)


(asdf:load-system :clasp-cleavir)

(generate-hir-for-clasp-source)
*hir-single-step*

(hir-form 1)
(translate *hir*)

*basic-blocks*
*tags*
*vars*




(hir-form '(lambda (x &optional (y 0)) (+ x y)))

(draw-hir)

(draw-mir)

*mir*


(core:getpid)

(hir-form '(let ((y 100) (z 200) ) #'(lambda (x) (list x y z))))

(defparameter *a* 1)
(defparameter *b* 2)

(mir-form '(lambda (x y) (list x y)))
(hir-form '(let ((x 100)) (tagbody top (setq x (1- x)) (if (eql x 0) (go done)) (go top) done)))
*hir*

(hir-form 1)

(defun foo (x x) x)

(foo 1 2)





(cleavir-compile 'tcl '(lambda (max) (let ((x 0)) (dotimes (i max) (setq x (+ i x ))) (print x))))

(tcl 10)




(defmacro defun (&whole whole name vl &body body &environment env)
  ;; Documentation in help.lsp
  (multiple-value-bind (decls body doc-string) 
      (core:process-declarations body t)
    (let* ((doclist (when doc-string (list doc-string)))
	   (global-function (compile nil `(lambda ,vl 
					    (declare (core:lambda-name ,name) ,@decls) 
					    ,@doclist (block ,(si::function-block-name name) ,@body)))))
      ;;(bformat t "DEFUN global-function --> %s\n" global-function )
      `(progn
	 ,(ext:register-with-pde whole `(si::fset ',name ,global-function))
	 ,@(si::expand-set-documentation name 'function doc-string)
	 ',name))))
(defun a (x y) (+ x y))


(room)


(llvm-sys:cxx-data-structures-info)




(defclass a () ((x :initarg :x :accessor x)))
(defclass b (a) ((y :initarg :y :accessor y)))
(defgeneric save-info (x) (:method-combination append :most-specific-last))
(defmethod save-info append ((o a)) '(:x x))
(defmethod save-info append ((o b)) '(:y y))

(defparameter *a* (make-instance 'a :x 1))
(defparameter *b* (make-instance 'b :x 2 :y 3))
(trace clos::effective-method-function)
(trace si:coerce-to-function)
(trace clos::combine-method-functions)
(trace cl:coerce)
(save-info *a*)
(save-info *b*)

(trace sb-pcl::make-effective-method-function)
(apropos "effective-method-function")



(load "sys:kernel;cleavir;cmpclasp.lisp")
(clasp-cleavir:cleavir-compile 'tdb '(lambda () (destructuring-bind (x &optional y) '(1) (print (list x y)))) :debug t)
(setq cmp:*low-level-trace-print* t)
(tdb)

(clasp-cleavir:cleavir-compile 'top '(lambda (x &optional y) (print (list x y))))
(top 1)


(clasp-cleavir:cleavir-compile 'tconsp '(lambda (x) (consp x)) :debug t)
(clasp-cleavir:cleavir-compile 'tcar '(lambda (x) (car x)) :debug t)
(clasp-cleavir:cleavir-compile 'tcdr '(lambda (x) (cdr x)) :debug t)





(clasp-cleavir:cleavir-compile
 'ttest
 '(lambda ()
   (LET* ((input-list '(1))
          (temp-list input-list)
          (X
           (PROGN
             (IF (NULL temp-list) (CORE::DM-TOO-FEW-ARGUMENTS input-list))
             (PROG1 (CAR (TRULY-THE CONS temp-list))
               (SETQ temp-list (CDR (TRULY-THE CONS temp-list))))))
          (Y
           (progn
             (print (list "temp-list" temp-list))
             (IF temp-list
                 (PROG1 (CAR (TRULY-THE CONS temp-list))
                   (print "Evaluated car the second time")
                   (SETQ temp-list (CDR (TRULY-THE CONS temp-list))))
                 (progn
                   (print "returning nil")
                   NIL)))))
     (DECLARE (IGNORABLE temp-list input-list))
;;     (print (list "y = " y))
     (IF temp-list (CORE::DM-TOO-MANY-ARGUMENTS input-list))
     (LIST X Y))) :debug t)




(clasp-cleavir:cleavir-compile
 'ttest2
 '(lambda ()
   (LET* ((temp-list '())
          (Y
           (IF temp-list
               (PROG1 (CAR (TRULY-THE CONS temp-list)g)
                 (SETQ temp-list (CDR (TRULY-THE CONS temp-list))))
               NIL)))
     (DECLARE (IGNORABLE temp-list ))
     ;;     (print (list "y = " y))
     (IF temp-list (CORE::DM-TOO-MANY-ARGUMENTS temp-list))
     (LIST Y))) :debug nil)

(clasp-cleavir:with-debug-compile-file ("/tmp/tdl.log" :debug-log-on t)
                          (clasp-cleavir:cleavir-compile-file "sys:tests;tdl.lsp"))

(clasp-cleavir:cleavir-compile
 'ttest3
 '(lambda ()
   (LET* ((temp-list '())
          (Y
           (IF temp-list
               (PROG1 (CAR (TRULY-THE CONS temp-list))
                 (SETQ temp-list (CDR temp-list)))
               NIL)))
     (DECLARE (IGNORABLE temp-list ))
     ;;     (print (list "y = " y))
     (LIST Y))) :debug t)

(clasp-cleavir:cleavir-compile
 'tctt
 '(lambda ()
   (car (truly-the cons (cdr '(1))))))
                               

(load "sys:kernel;cleavir;cmpclasp.lisp")
(clasp-cleavir:with-debug-compile-file ("/tmp/tdl/dl.log" :debug-log-on t)
  (clasp-cleavir:cleavir-compile-file "sys:tests;tdl3.lsp"))


(clasp-cleavir:cleavir-compile
 'foo
 '(lambda ()
   (LET* ((temp-list '())
          (Y (IF temp-list
                 (LET ((p1val (CAR TEMP-LIST))) p1val)
                 99)))
     (print Y))
   ) :debug t)




(defgeneric foo (x y))

(defmethod foo (x y) (+ x y))

(defun bar (x y) (+ x y))

(time (dotimes (i 1000000) (foo 1 2)))
(time (dotimes (i 1000000) (bar 1 2)))
(time (dotimes (i 1000000) ))
(core:tpl-backtrace)



(deftype boolean ()
  "A BOOLEAN is an object which is either NIL or T."
  '(member nil t))
