
(in-package :ffi)

#+clasp
(defmacro c-inline (fn-name (&rest values) (&rest c-types) return-type C-code &key one-liner side-effects)
  `(,fn-name ,@values))
(export 'c-inline)


(in-package :ext)
(defmacro ext::special-var (name)
  `(ext::special-var ,name))

(defmacro ext::lexical-var (name depth index)
  `(ext::lexical-var ,name ,depth ,index))



;;
;; Some helper macros for working with iterators
;;
;;

(in-package :ext)

(defmacro do-c++-iterator ((i iterator) &rest body)
  (let ((begin-gs (gensym))
        (end-gs (gensym))
        (cur (gensym)))
    `(multiple-value-bind (,begin-gs ,end-gs)
         ,iterator
       (do* ((,cur ,begin-gs (sys:iterator-step ,cur))
             (,i (sys:iterator-unsafe-element ,cur) (sys:iterator-unsafe-element ,cur)))
            ((eql ,cur ,end-gs))
         ,@body
         )
       )))


(defmacro map-c++-iterator (code iterator)
  (let ((val (gensym)))
    `(progn
       (do-c++-iterator (,val ,iterator) (funcall ,code ,val))
       nil)))

(export '(do-c++-iterator map-c++-iterator))


(in-package :cl)

(defmacro unwind-protect (protected-form &rest cleanup-forms)
  `(core:funwind-protect (lambda () ,protected-form) (lambda () ,@cleanup-forms)))

(defmacro catch (tag &rest forms)
  `(core:catch-function ,tag (lambda () (declare (core:lambda-name catch-lambda)) ,@forms)))

(defmacro throw (tag result-form)
  `(core:throw-function ,tag (lambda () (declare (core::lambda-name throw-result-lambda)) ,result-form)))

#+(or)
(defmacro multiple-value-call (function &rest forms)
  (if (= (length forms) 1)
      `(core:multiple-value-one-form-call ,function ,(car forms))
      `(core:multiple-value-funcall
        ,function
        ,@(mapcar (lambda (x) `#'(lambda () (progn ,x))) forms))))



       
#+(or)
(defmacro progv (symbols values &rest forms)
  `(core:progv-function ,symbols ,values #'(lambda () ,@forms)))



(in-package :core)
#+clasp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO is needed by sp_macrolet and the compiler
;;;
(defun parse-macro (name vl body &optional env)
  (multiple-value-bind (lblock ppn doc)
      (si::expand-defmacro name vl body)
    lblock))

(export 'parse-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core:debug-message is a macro to mimic the core:debug-message special operator
;;;
(defmacro debug-message (msg) nil)
(export 'debug-message)



(in-package :core)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the fdefinition for all special operators to something more reasonable than T
;;;
(dolist (so (core::list-of-all-special-operators))
  (when (eq (fdefinition so) T)
    (core:fset so
                (let ((so so))
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error 'do-not-funcall-special-operator :operator so))))))

(export 'do-not-funcall-special-operator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reader macro for builtin objects
;;
(defun read-cxx-object (stream char n)
  (declare (ignore char))
  (let ((description (read stream t nil t)))
    (apply #'core:make-cxx-object (car description) (cdr description))))

(set-dispatch-macro-character #\# #\I #'read-cxx-object)

(defmacro with-print-readably (&rest body)
  `(with-standard-io-syntax
     (let ((*print-circle* t))
       ,@body)))


(defmacro interpreter-trace (name)
  (if (null *interpreter-trace*)
      (setq *interpreter-trace* (make-hash-table)))
  (hash-table-setf-gethash *interpreter-trace* name t))

(defmacro interpreter-untrace (name)
  (if *interpreter-trace*
      (remhash name *interpreter-trace*)))
