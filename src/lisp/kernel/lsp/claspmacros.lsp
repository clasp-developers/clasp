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
(defmacro progv (symbols values &rest forms)
  `(core:progv-function ,symbols ,values #'(lambda () ,@forms)))

(in-package :core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core:debug-message is a macro to mimic the core:debug-message special operator
;;;
(defmacro debug-message (msg) nil)
(export 'debug-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the fdefinition for all special operators to something more reasonable than T
;;;
(dolist (so (core::list-of-all-special-operators))
  (when (null (fboundp so))
    (core:fset so
                (let ((so so))
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error 'do-not-funcall-special-operator :operator so :name so))))))

(export 'do-not-funcall-special-operator)

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

(defun do-memory-ramp (closure pattern)
  (unwind-protect
       (progn
         (gctools:alloc-pattern-begin pattern)
         (funcall closure))
    (gctools:alloc-pattern-end)))

(defmacro with-memory-ramp ((&key (pattern 'gctools:ramp)) &body body)
  `(if (member :disable-memory-ramp *features*)
       (progn
         (core:bformat t "Compiling with memory-ramp DISABLED%N")
         (funcall (lambda () (progn ,@body))))
       (do-memory-ramp (lambda () (progn ,@body)) ,pattern)))

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


(defmacro with-metrics-message ((message) &body body)
  `(unwind-protect
        (progn
          (core:monitor-write (core:bformat nil "{{{ %s%N" ,message))
          ,@body)
     (core:monitor-write (core:bformat nil "}}} ;;; %s%N" ,message))))

(export 'with-metrics-message)

#|
(define-compiler-macro new-apply (function-desig &rest args)
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


(define-compiler-macro apply-general (function-design &rest args)
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

