;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Running slime from bclasp+cleavir - don't load inline.lsp or auto-compile
;;;  --- Testing defun-inline-hook

(time (compile-file "sys:modules;asdf;build;asdf.lisp"))
(progn ;; Set up everything for building cclasp from bclasp with auto-compile
  (format t "Loading ASDF system~%")
  (finish-output)
  (time (load "sys:modules;asdf;build;asdf.fasl"))
  #+(or)(time (require :asdf))
  (load "sys:local-asdf-config.lisp"))
(progn
  #+(or)(core::cclasp-features)
  (format t "Loading :clasp-cleavir system~%")
  (finish-output)
  (time (asdf:load-system "clasp-cleavir"))
  (format t "Done  pid = ~a~%"  (core:getpid)))

(apropos "dump-module")

(let ((compiler:*compile-file-debug-dump-module* t)) (clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;pprint.lsp"))

(clasp-cleavir:cleavir-compile-file "sys:kernel;lsp;pprint.lsp")

(clasp-cleavir:cleavir-compile-file "sys:tests;tc.lsp")


(in-package :core)
(clasp-cleavir:cleavir-compile 'foo '(lambda (kind &optional stream)
                                      (declare (type (member :linear :miser :fill :mandatory) kind)
                                       (type (or stream (member t nil)) stream)
                                       (values null)
                                       (ext:check-arguments-type)
                                       #.+ecl-safe-declarations+)
                                      (let ((stream (case stream
                                                      ((t) *terminal-io*)
                                                      ((nil) *standard-output*)
                                                      (t stream))))
                                        (when (and (pretty-stream-p stream) *print-pretty*)
                                          (enqueue-newline stream kind)))
                                      nil)
                               :debug t)








(clasp-cleavir:cleavir-compile 'foo '(lambda (x) (declare (optimize (safety 1) (speed 0))) (car (the cons x))) :debug t)

(clasp-cleavir:cleavir-compile nil '(lambda (x) (declare (optimize (safety 0) (speed 1))) (car (the cons x))) :debug t)

(foo :x)

(time (asdf:load-system "clasp-cleavir"))

(clasp-cleavir:cleavir-compile-file "sys:tests;td.lsp")


(time (require :clasp-cleavir))

(load "sys:kernel;cleavir;auto-compile.lisp")
(load "sys:kernel;cleavir;inline.lisp")


(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))
(foo 1 2)

(let ((cmp:*compile-file-debug-dump-module* t)) (clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp"))
(load "sys:tests;ta.fasl")
(foo)

(trace clasp-cleavir-ast:hoist-load-time-value)


(foo 0)
(let ((cmp:*compile-debug-dump-module* t)) (clasp-cleavir:cleavir-compile 'foo '(lambda (x) (declare (core:lambda-name foo)) (list x x))))
(disassemble 'foo)

(fdefinition 'foo)
(clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (declare (fixnum x y)) (if (< x y) 1 2)))


*features*
(clasp-cleavir:cleavir-compile-file "sys:modules;asdf;build;asdf.lisp" :print t)
(clasp-cleavir:cleavir-compile-file "~/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Type-inference/type-descriptor.lisp" :print t)

(print "Hello there")

(defun my-hir-form (form)
  (let* ((ast (cleavir-generate-ast:generate-ast
               form
               clasp-cleavir:*clasp-env*
               clasp-cleavir:*clasp-system*))
         (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
         (init-instr (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
         (clasp-inst *clasp-system*))
    (clasp-cleavir:convert-funcalls init-instr)
    ;; my-hir-transformations
    (draw-hir init-instr #P"/tmp/hir-before.dot") ;; comment out
    (let ((types (cleavir-type-inference:infer-types init-instr)))
      (cleavir-type-inference::prune-typeqs init-instr types)
      (cleavir-type-inference::delete-the init-instr)
      (cleavir-hir-transformations:process-captured-variables init-instr)
      (draw-hir init-instr #P"/tmp/hir-after-ti.dot") ;; comment out
      #+(or)(clasp-cleavir::draw-hir init-instr))))

(clasp-cleavir:cleavir-compile
 'bar
 '(lambda (x)
   (if (cleavir-primop:typeq x fixnum)
       (if (cleavir-primop:typeq x fixnum)
           10
           20)
       25)) :debug t)

clasp-cleavir::*hir-types*
(clasp-cleavir:cleavir-compile
 'foo
 '(let ((a 1))
   (lambda (x)
     (declare (fixnum x))
     (if (cleavir-primop:typeq x fixnum)
         a
         20)))
 :debug t)

(require :cleavir-type-inference)
(defparameter *x*
  (my-hir-form '(let ((a 1))
                 (lambda (x)
                   (declare (fixnum x))
                   (if (cleavir-primop:typeq x fixnum)
                       a
                       20)))))


(clasp-cleavir:cleavir-compile-file "sys:tests;tt.lsp" :print t)






  (let* ((clasp-system *clasp-system*)
         (ast (cleavir-generate-ast:generate-ast form env clasp-system))
         (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
         (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast)))
    (clasp-cleavir:convert-funcalls hir)
    (my-hir-transformations hir clasp-system nil nil)
    (cleavir-ir:hir-to-mir hir clasp-system nil nil) 
    (clasp-cleavir:optimize-stack-enclose hir)
    (cc-mir:assign-mir-instruction-datum-ids hir)
    (clasp-cleavir:finalize-unwind-and-landing-pad-instructions hir)
    (translate hir abi))


  asdf:*central-registry*




  (defun zzz (x y z) (+ x y z))
  (fdefinition 'zzz)


  (progn
    (load "sys:kernel;cleavir;auto-compile.lisp")
    (format t "Loading inline.lisp~%")
    (load "sys:kernel;cleavir;inline.lisp")
    (format t "Done loading inline.lisp~%"))

  (clasp-cleavir:cleavir-compile 'foo '(lambda (x y) (+ x y)))
  (clasp-cleavir:cleavir-compile-file "sys:tests;ta.lsp")
  (load "sys:tests;ta.fasl")
  (baz 1 2)
  (clasp-cleavir:cleavir-compile 'foo '(lambda (x &va-rest y) (apply #'list x y)))





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
   (time (bar 1))

   :r1


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
   (trace clasp-cleavir::cclasp-compile*)
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


