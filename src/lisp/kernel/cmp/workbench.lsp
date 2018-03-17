
;;; Test core:bind-va-list special operator

(in-package :cmp)

(setq cmp:*compile-debug-dump-module* t)
(defun foo (core:&va-rest r) (core:bind-va-list (x y) r (list x y)))
(defun foo (core:&va-rest r) (core::bind-va-list (x &optional (y :testing)) r (list x y)))
(defun foo (core:&va-rest r) (core:bind-va-list (x &rest y) r (list* x y)))
(defun foo (x core:&va-rest r) (core:bind-va-list (x &rest y) r (unless (eq x 0) (setq x 0)) (let ((z (list* x y))) (print z) z)))


(clasp-cleavir:cleavir-compile 'foo '(lambda (core:&va-rest r) (core::bind-va-list (x y z &rest a) r (list* z y x a))))
(defparameter *f* '(lambda (core:&va-rest r) (core::bind-va-list (x &rest y) r (list* x y))))
(defparameter *a* (cleavir-generate-ast:generate-ast *f* clasp-cleavir::*clasp-env* clasp-cleavir::*clasp-system*))
(cleavir-ast-to-hir:compile-toplevel-unhoisted *a*)

(clasp-cleavir:cleavir-compile 'foo *f*)



(trace treat-as-special-operator)

(apropos "&va-rest")




;;;  Fastgf stuff





(in-package :cl-user)

(progn
  (defgeneric foo (x))
  (defmethod foo ((x integer)) :integer)
  (defmethod foo ((x float)) :float)
  (defmethod foo ((x string)) :string)
  (foo 1)
  (foo 1.2)
  (foo "asdf"))


(clos::calculate-fastgf-dispatch-function #'foo)

(clos:switch-to-fastgf #'foo)

(clos:get-funcallable-instance-function #'foo)

(foo 1.2)
(foo 1)
(foo "asdfasdf")


(progn
  (defgeneric bar (x))
  (defmethod bar ((x integer)) :integer)
  (defmethod bar ((x float)) :float)
  (defmethod bar ((x string)) :string)
  (bar 1)
  (bar 1.2)
  (bar "asdf"))

(print "Hello")
(time (progn (print "foo") (dotimes (i 1000000) (foo 1))))

(print  "Hello")

(trace compile-file)
(trace cmp::compile-file-to-module)
(trace clasp-cleavir::cleavir-compile-file-form)
(trace clasp-cleavir::compile-form)
(trace clasp-cleavir::translate)
(trace clasp-cleavir::compile-form-to-mir)
(trace cleavir-generate-ast:generate-ast)
(trace clasp-cleavir-ast:hoist-load-time-value)
(trace cleavir-ast-to-hir:compile-toplevel)
(trace clasp-cleavir:convert-funcalls)
(trace clasp-cleavir::my-hir-transformations)
(trace clasp-cleavir::quick-draw-hir)
(trace cleavir-ir:hir-to-mir)
(trace cc-mir:assign-mir-instruction-datum-ids)
(trace clasp-cleavir:finalize-unwind-and-landing-pad-instructions)
(trace cmp::jit-add-module-return-function)

(trace cleavir-generate-ast::convert)
(trace cleavir-generate-ast::convert-special)

(print "hello")
(time (progn (print "bar") (dotimes (i 1000000) (bar 1))))

;;; --------------------------------------------------
;;;
;;; Work with metering literal compilation
;;;


(mon:with-monitoring (literal::ltv/cons
                      literal::reference-literal
                      literal::compile-reference-to-literal
                      clasp-cleavir::%literal-index
                      clasp-cleavir::%literal-ref
                      clasp-cleavir::%literal-value
                      clasp-cleavir::%literal
                      compile-file
                      ) ()
  (compile-file "sys:tests;biglists.lsp" :output-type :bitcode))

(time
 (let ((cmp::*link-options* (list "-O0")))
   (compile-file "sys:tests;biglists.lsp")))

(time
 (let ((cmp::*link-options* (list "-O2")))
   (compile-file "sys:tests;biglists.lsp")))

(time
 (let ((*features* (cons :debug-run-clang *features*))
       (cmp::*link-options* (list "-O2" "--time-passes")))
   (compile-file "sys:tests;biglists20000.lsp")))

;;; --------------------------------------------------
;;; Work with generic function dispatch
;;;
(dolist (x (clos::all-generic-functions))
;;;  (core:bformat t "%s\n" x)
  (if (member x (list
                 #'clos::compute-applicable-methods-using-classes
                 #'clos::add-direct-method
#|
                 #'class-name
                 #'initialize-instance
                 #'clos:add-direct-subclass
                 #'clos:validate-superclass
|#
                      ))
;;;      (core:bformat t "     Skipping\n")
      (progn
        (clos::update-specializer-profile x)
        (clos::switch-to-fastgf x))))



(let ((dispatchers (make-hash-table)))
  (dolist (x (clos::all-generic-functions))
    (clos::update-specializer-profile x)
    (if (member x (list
                   #'clos::compute-applicable-methods-using-classes
                   #'clos::add-direct-method
                   #'clos::compute-effective-method
                   ))
        nil
        (setf (gethash x dispatchers) (clos::calculate-fastgf-dispatch-function x))))
  (maphash (lambda (gf disp)
             (clos::safe-set-funcallable-instance-function gf disp))
           dispatchers))

;;;  (core:bformat t "%s\n" x)
  (if (member x (list
                 #'clos::compute-applicable-methods-using-classes
                 #'clos::add-direct-method
#|
                 #'class-name
                 #'initialize-instance
                 #'clos:add-direct-subclass
                 #'clos:validate-superclass
|#
                      ))
;;;      (core:bformat t "     Skipping\n")
      (progn
        (clos::update-specializer-profile x)
        (clos::switch-to-fastgf x))))




(clos::generic-function-call-history #'clos::add-direct-method)

(setq *print-pretty* nil)

(dolist (x (clos::optimized-call-history #'clos::add-direct-method)) (print (car x)))
(dolist (x (clos::generic-function-call-history #'clos::add-direct-method)) (print (car x)))


(trace cmp::node-add cmp::node-class-add)
(clos::update-specializer-profile #'clos::add-direct-method)
(clos::graph-fastgf-dispatch-function #'clos::add-direct-method)
(clos::generic-function-specializer-profile #'clos::add-direct-method)






(progn
;  (setq clos::*enable-fastgf* t)
  (defgeneric foo (w x y))
  (defmethod foo (w (x (eql 'a)) y) :integer)
  (defmethod foo (w (x (eql 'b)) y) :symbol)
  (defmethod foo (w (x (eql 'c)) y) :string))

(foo 8 'a 1 )
(foo 8 'a :asdf )
(foo 8 'b "asdf" )

(clos::switch-to-fastgf #'foo)

(foo 8 'c 1)

(graph-fastgf-dispatch-function #'foo)

(defmethod foo ((w string) x y) :goofy)
(foo "abcd" 'c 1.2)
(graph-fastgf-dispatch-function #'foo)

(clos::get-funcallable-instance-function #'foo)



(apropos "node")





(load "sys:kernel;cmp;jit-setup.lsp")
(load "sys:kernel;cmp;cmpgf.lsp")


(defmethod foo ((x integer)) :integer)
(foo 1)
(clos:switch-to-fastgf #'foo)
(defmethod foo ((x symbol)) :symbol)

(foo 1.2)

(defclass bar () ((barx :initarg :barx :accessor barx)))
(defmethod foo ((x bar)) :bar)
(defparameter b (make-instance 'bar :barx 1))
(foo b)
(foo 1.2)


(clos:get-funcallable-instance-function #'foo)


(defclass bar () ((barx :initarg :barx :accessor barx)
                  (bary :initarg :bary :accessor bary)))

(defclass baz () ((barx :initarg :barx :accessor barx)
                  (bazy :initarg :bazy :accessor bazy)))







(progn
  (defclass bar () ())
  (defclass baz (bar) ())
  (defmethod foo ((x baz)) :baz)
  (defmethod foo ((x string)) :string)
  (defmethod foo ((x float)) :float)
  (defmethod foo ((x integer)) :integer)
  (defmethod foo ((x (eql 42))) :forty-two)
  )

(progn
  (foo "abc")
  (foo 1)
  (foo 42)
  (foo (make-instance 'baz)))

(clos::switch-to-strandh-dispatch #'foo)
(clos::generic-function-call-history #'foo)

(clos::switch-to-strandh-dispatch #'foo)

(foo 42)
(foo (make-instance 'baz))

(time (dotimes (i 10000000) (foo 1.2)))

(defun bar (x) :bar)
(time (dotimes (i 10000000) (bar 1.2)))

(defmethod wizbang (x) :wizbang)
(wizbang 1.2)
(time (dotimes (i 10000000) (wizbang 1.2)))



(print (clos::generic-function-call-history #'foo))
(print (clos::valid-call-history-after-class-change #'foo (find-class 'bar)))
(clos:call-history-entry-key-contains-specializer (car (car (clos::generic-function-call-history #'foo))) 

(clos::calculate-strandh-dispatch-function #'foo)

(foo "abc")
(foo 123.3)
(clos::generic-function-call-history (fdefinition 'foo))
(mapc (lambda (x) (format t "Switching ~a~%" x) (clos::switch-to-strandh-dispatch x)) (clos::all-generic-functions))


(clos:compute-effective-method #'clos:compute-effective-method (clos:generic-function-method-combination #'clos:compute-effective-method) (clos:generic-function-methods #'clos:compute-effective-method))

(in-package :clos)

(time
 (progn
   ;; Call c-a-m-u-c once to establish a call-history.  ECL bypasses it
   (clos::generic-function-compiled-dispatch-function #'print-object)
   (setf (clos::generic-function-compiled-dispatch-function #'print-object) t)
   (clos::compute-applicable-methods-using-classes #'print-object (list (find-class T) (find-class 'stream)))
   (clos:compute-effective-method #'clos:compute-effective-method (clos:generic-function-method-combination #'clos:compute-effective-method) (clos:generic-function-methods #'clos:compute-effective-method))
   (clos::generic-function-name #'print-object)
   (print-object #'print-object *standard-output*)
   (print-object (core::make-restart) *standard-output*)
   (defparameter *newgfs* (mapcar (lambda (gf) (cons gf (clos::calculate-strandh-dispatch-function gf))) (clos::all-generic-functions)))
   (dolist (ng *newgfs*) (clos:set-funcallable-instance-function (car ng) (cdr ng)))))


(progn
  (defparameter *all-histories* (make-hash-table))
  (defun record-call-histories ()
    (dolist (gf (clos::all-generic-functions))
      (setf (gethash gf *all-histories*) (clos::generic-function-call-history gf))))
  (defun diff-call-histories ()
    (dolist (gf (clos::all-generic-functions))
      (let ((old-history (gethash gf *all-histories*)))
        (when (not (equalp old-history (clos::generic-function-call-history gf)))
          (format t "Call history changed for: ~a~%" gf)
          (format t "     Original call-history: ~a~%" old-history)
          (format t "          New call-history: ~a~%" (clos::generic-function-call-history gf)))))))
          

(defclass foo () ())
(make-instance 'foo)



(clos::get-funcallable-instance-function (fdefinition 'foo))

;;; ensure-outcome
(foo "abc")
(foo 1)
(foo 1.2)
(foo 42)
(foo (make-instance 'baz))




(progn
  (defgeneric zap (x))
  (defmethod zap ((x (eql 1))) 1)
  (defmethod zap ((x (eql 2))) 2)
  )

(clos::switch-to-strandh-dispatch #'zap)

(zap 1)
(zap 2)
(zap 3)




(progn
  (defclass foo () ())
  (defclass foo-a (foo) ())
  (defclass foo-b (foo) ())
  (defclass bar (foo-a foo-b) ())
  )

(progn
  (defmethod zap ((x foo-a)) :zip-foo-a)
  (defmethod zap ((x foo-b)) :zip-foo-b))

(clos:compute-applicable-methods-using-classes #'zap (list (find-class 'bar)))
(find-class 'bar)


(trace clos::sort-applicable-methods)
(trace clos::compare-methods)
(trace clos::compare-specializers)
(trace clos::compare-specializers-lists)

(zap (make-instance 'bar))
(sb-mop:compute-applicable-methods-using-classes #'zap (list (find-class 'bar)))

(clos::compare-specializers (find-class 'foo-a) (find-class 'foo-b) (find-class 'bar))


(zip (make-instance 'bar))
