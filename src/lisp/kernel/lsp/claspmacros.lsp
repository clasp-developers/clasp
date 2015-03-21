
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
  `(core:catch-function ,tag (lambda () ,@forms)))

(defmacro throw (tag result-form)
  `(core:throw-function ,tag ,result-form))


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

