
(load "sys:kernel;cmp;jit-setup.lsp")
(load "sys:kernel;cmp;cmpgf.lsp")
(defmethod foo ((x integer)) :integer)
(foo 1)
(clos:switch-to-fastgf #'foo)
(clos:get-funcallable-instance-function #'foo)








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
(core:call-history-entry-key-contains-specializer (car (car (clos::generic-function-call-history #'foo))) 

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
