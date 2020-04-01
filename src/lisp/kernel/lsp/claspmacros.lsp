(in-package :ext)
(defmacro ext::special-var (name)
  `(ext::special-var ,name))

(defmacro ext::lexical-var (name depth index)
  `(ext::lexical-var ,name ,depth ,index))

;;; to work with fpe-exceptions
(defun ext::get-fpe-parameters (traps all-traps)
;;; (loop for trap in all-traps collect trap collect (if (find trap traps) nil t))
  (let ((result nil))
    (dolist (trap all-traps)
      (push trap result)
      (push (if (find trap traps) nil t) result))
    (reverse result)))

(defmacro ext:with-float-traps-masked (traps &body body)
  (let ((previous (gensym "PREVIOUS"))
        (all-traps '(:underflow :overflow :inexact :invalid :divide-by-zero :denormalized-operand)))
    `(let ((,previous (core::get-current-fpe-mask)))
       (unwind-protect
            (progn
              (core::enable-fpe-masks
               ,@(ext::get-fpe-parameters traps all-traps))
               ,@body)
              (core::set-current-fpe-mask ,previous)))))

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

(in-package :core)

(defmacro cl:unwind-protect (protected-form &rest cleanup-forms)
  `(core:funwind-protect (lambda () ,protected-form) (lambda () ,@cleanup-forms)))

(defmacro cl:catch (tag &rest forms)
  `(core:catch-function ,tag (lambda () (declare (core:lambda-name catch-lambda)) ,@forms)))

(defmacro cl:throw (tag result-form)
  `(core:throw-function ,tag (lambda () (declare (core::lambda-name throw-result-lambda)) ,result-form)))
  
#+(or)
(defmacro cl:progv (symbols values &rest forms)
  `(core:progv-function ,symbols ,values #'(lambda () ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core:debug-message is a macro to mimic the core:debug-message special operator
;;;
(defmacro debug-message (msg) nil)
(export 'debug-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the fdefinition for all special operators to something more reasonable than T
;;; For known operators, put in a function with correct lambda list for the sake of
;;; documentation.
;;;
(defmacro def-special-operator-function (name lambda-list &optional (vars lambda-list))
  `(unless (fboundp ',name)
     (core:fset ',name
                (lambda ,lambda-list
                  (declare (ignore ,@vars))
                  (error 'do-not-funcall-special-operator :operator ',name)))))
(def-special-operator-function progn (&rest forms) (forms))
(def-special-operator-function block (name &rest forms) (name forms))
(def-special-operator-function catch (tag &rest forms) (tag forms))
(def-special-operator-function eval-when (situations &rest forms) (situations forms))
(def-special-operator-function flet (bindings &rest body) (bindings body))
(def-special-operator-function function (thing))
(def-special-operator-function the (values-type form))
(def-special-operator-function go (tag))
(def-special-operator-function if (condition then else))
(def-special-operator-function labels (bindings &rest body) (bindings body))
(def-special-operator-function let (bindings &rest body) (bindings body))
(def-special-operator-function let* (bindings &rest body) (bindings body))
(def-special-operator-function locally (&rest body) (body))
(def-special-operator-function macrolet (bindings &rest body) (bindings body))
(def-special-operator-function multiple-value-prog1 (values-form &rest forms) (values-form forms))
(def-special-operator-function multiple-value-call (function &rest args) (function args))
(def-special-operator-function progv (symbols values &rest forms) (symbols values forms))
(def-special-operator-function quote (object))
(def-special-operator-function return-from (name &optional value) (name value))
(def-special-operator-function setq (&rest pairs) (pairs))
(def-special-operator-function tagbody (&rest statements) (statements))
(def-special-operator-function throw (tag result-form))
(def-special-operator-function unwind-protect (protected &rest cleanup) (protected cleanup))
(def-special-operator-function symbol-macrolet (bindings &rest body) (bindings body))
(def-special-operator-function load-time-value (form &optional read-only-p) (form read-only-p))
(dolist (so (core::aclasp-list-of-all-special-operators))
  (when (null (fboundp so))
    (core:fset so
                (let ((so so))
                  (lambda (&rest args)
                    (declare (ignore args))
                    (error 'do-not-funcall-special-operator :operator so))))))

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


(defmacro with-monitor-message-scope ((fmt &rest args) &body body)
  #+debug-monitor
  (let ((msg (gensym)))
    `(let ((,msg (format nil ,fmt ,@args)))
       (unwind-protect
            (progn
              (core:monitor-write (core:bformat nil "{{{ ;;; %s%N" ,msg))
              ,@body)
         (core:monitor-write (core:bformat nil "}}} ;;; %s%N" ,msg)))))
  #-debug-monitor
  nil)

(defmacro monitor-message (fmt &rest args)
  #+debug-monitor
  `(sys:monitor-write (core:bformat nil "%s%N" (format nil ,fmt ,@args)))
  #-debug-monitor
  nil)

(export '(with-monitor-message-scope monitor-message))

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
