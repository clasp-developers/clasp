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

(defmacro do-c++-iterator ((i iterator &optional (cur (gensym)) (begin (gensym)) (end (gensym))) &rest body)
  `(multiple-value-bind (,begin ,end)
                        ,iterator
                        (do* ((,cur ,begin (core:iterator-step ,cur))
                              (,i (core:iterator-unsafe-element ,cur) (core:iterator-unsafe-element ,cur)))
                             ((core:iterator= ,cur ,end))
                             ,@body)))


(defmacro map-c++-iterator (code iterator)
  (let ((val (gensym)))
    `(progn
       (do-c++-iterator (,val ,iterator) (funcall ,code ,val))
       nil)))

(export '(do-c++-iterator map-c++-iterator))


(defmacro with-locked-hash-table (ht &body body)
  "If the hash table is thread safe - then turn on the lock"
  (let ((htlock (gensym)))
  `(let ((,htlock (hash-table-shared-mutex ,ht)))
     (if ,htlock 
         (unwind-protect
              (progn
                (mp:shared-lock ,htlock)
                ,@body)
           (mp:shared-unlock ,htlock))
         (progn
           ,@body)))))

(export '(with-locked-hash-table))

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


;;;
;;; When threading is supported this macro should replicate the ECL mp:with-lock macro
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (find-package "MP")
      nil
      (make-package "MP" :use '(common-lisp))))

(in-package :mp)
(defmacro with-lock ((sym) &rest body)
  #+threading(warn "Make the mp:with-lock macro actually lock a symbol")
  `(progn ,@body))
(export 'with-lock)
(in-package :core)

#+(or)
(progn
  (declaim (inline char))
  (defun char (str idx)
    (declare (type (string str)))
    (if (eql safety 0)
        (if (or (minusp idx) (>= idx (length str)))
            (error "Index ~a must be positive and less than ~a" idx (length str))))
    (etypecase (str)
      (core:simple-base-char-string (intrinsic-call "SimpleBaseCharString_get" str idx))
      (core:simple-character-string (intrinsic-call "SimpleCharacterString_get" str idx))
      (core:str8-ns (intrinsic-call "Str8Ns_get" str idx))
      (core:str-w-ns (intrinsic-call "StrWNs_get" str idx)))))



#|
(core:bclasp-define-compiler-macro new-apply (function-desig &rest args)
  (let ((fun (gensym "FUN")))
    (if (> (length args) 8)
        `(let ((,fun ,function-desig))
           (core:multiple-value-foreign-call
            "fast_apply_general"
            (if (typep ,fun 'function)
                ,fun
                (if (typep ,fun 'symbol)
                    (symbol-function ,fun)
                    (error 'type-error :datum ,fun :expected-type '(or symbol function))))
            ,args))
        `(let ((,fun ))
           (core:multiple-value-foreign-call
            ,(bformat nil "fast_apply%d" (length args))
            (if (typep ,fun 'function)
                ,fun
                (if (typep ,fun 'symbol)
                    (symbol-function ,fun)
                    (error 'type-error :datum ,fun :expected-type '(or symbol function))))
            ,@args)))))


(core:bclasp-define-compiler-macro apply-general (function-design &rest args)
  `(let ((,fun ,function-desig))
     (core:multiple-value-foreign-call
      "fast_apply_general"
      (if (typep ,fun 'function)
          ,fun
          (if (typep ,fun 'symbol)
              (symbol-function ,fun)
              (error 'type-error :datum ,fun :expected-type '(or symbol function))))
      ,args)))
|#

