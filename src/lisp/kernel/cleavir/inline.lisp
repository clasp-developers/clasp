(in-package :clasp-cleavir)

#-bytecode
(progn

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to T~%")
  (setq core:*echo-repl-read* t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cmp::*debug-create-call* nil))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*defun-inline-hook* 'defun-inline-hook))

#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

#+(or)
(defmacro debug-inline (msg &rest msg-args)
  `(progn
     (core:fmt t "debug-inline>> ")
     (core:fmt t ,msg ,@msg-args)
     (core:fmt t "%N")
     (finish-output)))
(defmacro debug-inline (msg &rest msg-args)
  (declare (ignore msg msg-args))
  nil)

;;; This defines compiler macros that only come into effect when using cclasp.
;;; This is useful when their expansions involve cleavir-only special operators.
;;; Syntax is the same as define-compiler-macro, except that the lambda-list
;;; MUST start with (&whole something ...) for things to work.
;;; This macro is a little janky in that it doesn't work with declarations.
(defmacro define-cleavir-compiler-macro (name lambda-list &body body)
  (unless (and (consp lambda-list)
               (eq (first lambda-list) '&whole))
    (warn "BUG: Bad ~a for ~a" 'define-cleavir-compiler-macro name))
  `(define-compiler-macro ,name (,@lambda-list)
     ;; I just picked this since it's the first variable in hooks.lisp.
     (unless (eq cmp:*cleavir-compile-hook* 'bir-compile)
       (return-from ,(core:function-block-name name) ,(second lambda-list)))
     ,@body))

;;; If FORM is of the form #'valid-function-name, return valid-function-name.
;;; FIXME?: Give up on expansion and warn if it's invalid?
(defun constant-function-form (form env)
  (declare (ignore env))
  (and (consp form) (eq (first form) 'function)
       (consp (cdr form)) (null (cddr form))
       (core:valid-function-name-p (second form))
       (second form)))

(define-cleavir-compiler-macro funcall
    (&whole form function &rest arguments &environment env)
  ;; If we have (funcall #'foo ...), we might be able to apply the FOO compiler macro.
  ;; Failing that, we can at least skip any coercion - #'foo is obviously a function.
  ;; (funcall #'(setf foo) ...) is fairly common, so this is nice to do.
  (let ((name (constant-function-form function env)))
    (when name
      (return-from funcall
        (let* ((func-info (cleavir-env:function-info
                           clasp-cleavir:*clasp-system* env name))
               (notinline (and func-info
                               (eq 'notinline (cleavir-env:inline func-info))))
               ;; We can't get this from the func-info because it might be
               ;; a local-function-info, which doesn't have that slot.
               (cmf (compiler-macro-function name env)))
          (if (and cmf (not notinline))
              (funcall *macroexpand-hook* cmf form env)
              `(cleavir-primop:funcall ,function ,@arguments))))))
  `(cleavir-primop:funcall
    (core:coerce-called-fdesignator ,function)
    ,@arguments))

;;; We do this so that the compiler only has one form of variadic call to
;;; worry about. Also, perhaps counterintuitively, mv-call is actually
;;; easier for the compiler to work with than APPLY is, and more general
;;; besides.
(define-cleavir-compiler-macro apply (&whole form function &rest arguments)
  (if (null arguments)
      form ; invalid, let runtime handle the error
      `(multiple-value-call ,function
         ,@(loop for arg in (butlast arguments)
                 collect `(values ,arg))
         (values-list ,(first (last arguments))))))
#+(or)
(define-cleavir-compiler-macro values (&whole form &rest values)
  `(cleavir-primop:values ,@values))

;;; A compiler macro to avoid confusing bclasp.
(define-cleavir-compiler-macro ext:with-current-source-form
    (&whole f (&rest forms) &body body)
  `(cleavir-cst-to-ast:with-current-source-form (,@forms) ,@body))

;;; NOTE: The following two macros don't actually rely on anything cleavir-specific
;;; for validity. However, they do rely on their efficiency being from
;;; multiple-value-bind being efficient, which it is not without the above version.

(define-compiler-macro nth-value (&whole form n expr &environment env)
  (let ((n (and (constantp n env) (ext:constant-form-value n env))))
    (if (or (null n) (> n 100)) ; completely arbitrary limit
        form
        (let ((dummies (loop repeat n collect (gensym "DUMMY")))
              (keeper (gensym "SMARTIE")))
          `(multiple-value-bind (,@dummies ,keeper) ,expr
             (declare (ignore ,@dummies))
             ,keeper)))))

;;; This stupid little macro is to tighten up
;;; (if (and (fixnump x) (>= x c1) (< x c2)) ...)
;;; which is useful for bounds checks.
;;; The compiler can't optimize this condition very well as I write this.
;;; NOTE: Evaluates VAL more than once.
#+(or)
(defmacro if-in-bounds ((val low high) then else)
  `(core::local-block nil
     (if (core:fixnump ,val)
         (if (cleavir-primop:fixnum-not-less ,val ,low)
             (if (cleavir-primop:fixnum-less ,val ,high)
                 (return ,then)))
         (error 'type-error :datum ,val :expected-type 'fixnum))
     ,else))

(define-cleavir-compiler-macro cl:eq (&whole form x y)
  `(if (cleavir-primop:eq ,x ,y) t nil))

(declaim (ftype (function (t t) boolean) cl:eq eql))
#+(or)
(progn
  (debug-inline "eq")
  (defun cl:eq (x y)
    (if (cleavir-primop:eq x y) t nil)))
#+(or)
(progn
  (debug-inline "eql")
  (declaim (inline cl:eql))
  (defun eql (x y)
    (cond ((cleavir-primop:eq x y) t)
          ((typep x 'core::eq-incomparable)
           (if (typep y 'core::eq-incomparable)
               (core:eql-underlying x y)
               nil))
          (t nil))))

(declaim (ftype (function (&rest t) list) list))
(declaim (ftype (function (&rest t) list) list*))
(declaim (ftype (function (t t) cons) cons))

(declaim (ftype (function (list) t) car cdr))

;;; So. CAR and CDR are commonly used enough that doing a full function call for them
;;; noticeably slows down plenty of code (e.g. try cl-bench TAKL), even in case we
;;; have no type information about the list.
;;; But it's convenient for the compiler to just see CAR and CDR calls, since it can
;;; use the fancy inference on cons types (see type.lisp).
;;; So we provide this inline definition, but also put a notinline function call in
;;; it. The transform code (in bir-to-bmir.lisp) will take note of the declared type
;;; and reduce that "notinline" car to a primop, but that happens after type inference
;;; and so the compiler can use the type information.
;;; This is a KLUDGE, but it really seems to help in a lot of code.
#+(or)
(declaim (inline car cdr))
#+(or)
(defun car (list)
  "Return the first object in a list."
  (declare (optimize (safety 0)) (notinline car))
  (if (consp list)
      (car (the cons list))
      (if (eq list nil)
          (the null list)
          (error 'type-error :datum list :expected-type 'list))))
#+(or)
(defun cdr (list)
  "Return all but the first object in a list."
  (declare (optimize (safety 0)) (notinline cdr))
  (typecase list
    (cons (cdr (the cons list)))
    (null (the null list))
    (t (error 'type-error :datum list :expected-type 'list))))
#+(or)
(defmacro defcr (name &rest ops)
  `(progn
     (debug-inline ,(symbol-name name))
     (declaim (inline ,name)
              (ftype (function (list) t) ,name))
     (defun ,name (x)
       ,(labels ((rec (ops)
                   (if ops
                       `(,(first ops) ,(rec (rest ops)))
                       'x)))
          (rec ops)))))
#+(or)(progn
(defcr caar   car car)
(defcr cadr   car cdr)
(defcr cdar   cdr car)
(defcr cddr   cdr cdr)
(defcr caaar  car car car)
(defcr caadr  car car cdr)
(defcr cadar  car cdr car)
(defcr caddr  car cdr cdr)
(defcr cdaar  cdr car car)
(defcr cdadr  cdr car cdr)
(defcr cddar  cdr cdr car)
(defcr cdddr  cdr cdr cdr)
(defcr caaaar car car car car)
(defcr caaadr car car car cdr)
(defcr caadar car car cdr car)
(defcr caaddr car car cdr cdr)
(defcr cadaar car cdr car car)
(defcr cadadr car cdr car cdr)
(defcr caddar car cdr cdr car)
(defcr cadddr car cdr cdr cdr)
(defcr cdaaar cdr car car car)
(defcr cdaadr cdr car car cdr)
(defcr cdadar cdr car cdr car)
(defcr cdaddr cdr car cdr cdr)
(defcr cddaar cdr cdr car car)
(defcr cddadr cdr cdr car cdr)
(defcr cdddar cdr cdr cdr car)
(defcr cddddr cdr cdr cdr cdr)

(defcr rest    cdr)
(defcr first   car)
(defcr second  car cdr)
(defcr third   car cdr cdr)
(defcr fourth  car cdr cdr cdr)
(defcr fifth   car cdr cdr cdr cdr)
(defcr sixth   car cdr cdr cdr cdr cdr)
(defcr seventh car cdr cdr cdr cdr cdr cdr)
(defcr eighth  car cdr cdr cdr cdr cdr cdr cdr)
(defcr ninth   car cdr cdr cdr cdr cdr cdr cdr cdr)
(defcr tenth   car cdr cdr cdr cdr cdr cdr cdr cdr cdr)
)

;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;
#+(or)(progn
(debug-inline "array-total-size")
(declaim (inline array-total-size)
         (ftype (function (array) sys:index) array-total-size))
(defun array-total-size (array)
  (etypecase array
    ((simple-array * (*)) (core::primop core::vector-length array))
    ;; MDArray
    (array (core::primop core::%array-total-size array))))
)
#+(or)(progn
(debug-inline "array-rank")
(declaim (inline array-rank)
         (ftype (function (array) (integer 0 #.array-rank-limit)) array-rank))
(defun array-rank (array)
  (etypecase array
    ((simple-array * (*)) 1)
    (array (core::primop core::%array-rank array))))
)
#+(or)(progn
(declaim (inline schar (setf schar) char (setf char))
         (ftype (function (simple-string sys:index) character) schar)
         (ftype (function (string sys:index) character) char)
         (ftype (function (character simple-string sys:index) character) (setf schar))
         (ftype (function (character string sys:index) character) (setf char)))
(defun schar (string index)
  ;; We use DECLARE instead of THE here so as to skip needless
  ;; multiple-value checkers. cclasp is not yet smart enough to realize that in
  ;; (fun (the string foo)) the type check function for STRING does not need to
  ;; preserve non-primary values.
  (declare (type simple-string string))
  (row-major-aref string index))
(defun (setf schar) (value string index)
  (declare (type simple-string string))
  (setf (row-major-aref string index) value))

(defun char (string index)
  (declare (type string string))
  (row-major-aref string index))
(defun (setf char) (value string index)
  (declare (type string string))
  (setf (row-major-aref string index) value))

(defun row-major-index-computer (dimsyms subscripts)
  ;; assumes once-only is taken care of.
  ;; array is a symbol bound to an array.
  ;; dimsyms is a list of symbols bound to the array dimensions of the array.
  ;; subscripts is a list of symbols bound to the indices we're computing for.
  (let ((subsyms (loop for sub in subscripts collect (gensym "SUB"))))
    (cond
      ((null subscripts) '0)
      ;; special case the one dimensional case to avoid multiplication.
      ;; FIXME: That SHOULD be handled by constant propagation and all,
      ;; allowing this special case to be axed, but we don't have that yet.
      ((null (rest subscripts)) (first subscripts))
      (t
       `(let* ((,(first subsyms) 1) ;; this is the troubling constant.
               ,@(loop for dimsym in (reverse dimsyms)
                       for subsym in (cdr subsyms)
                       for lastsym in subsyms
                       collect `(,subsym (* ,lastsym ,dimsym))))
          (declare (type fixnum ,@subsyms))
          (+ ,@(loop for sub in subscripts
                     for subsym in (reverse subsyms)
                     collect `(* ,sub ,subsym))))))))
)
;;; ------------------------------------------------------------
;;;
;;; Sequence functions
;;;

(declaim (ftype (function (sequence) sys:index) length))

;;; Redefinition of C++ function.
;;; NOTE: This will be faster if we use a generic function or implement typecase
;;;  in terms of generic function dispatch.
#+(or)(progn
(declaim (inline core:coerce-fdesignator)
         (ftype (function ((or function symbol)) function)
                core:coerce-fdesignator))
(defun core:coerce-fdesignator (fdesignator)
  "Take a CL function designator and spit out a function."
  (etypecase fdesignator
    (function fdesignator)
    (symbol (fdefinition fdesignator))))
)
(declaim (ftype (function (t) function) core:coerce-to-function)))

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/pprint.lisp
;;;    and put here so that the inline definition is available
;;;
(in-package "SI")

#+(or)
(progn (declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (index-column (posn-index posn stream) stream))

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to NIL~%")
  (setq core:*echo-repl-read* nil)))

