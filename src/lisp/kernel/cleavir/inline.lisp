(in-package :clasp-cleavir)

(defpackage "PRIMOP"
  (:export #:inlined-two-arg-+
           #:inlined-two-arg--
           #:inlined-two-arg-*
           #:inlined-two-arg-/
           #:inlined-two-arg-<
           #:inlined-two-arg-<=
           #:inlined-two-arg-=
           #:inlined-two-arg->
           #:inlined-two-arg->=
           ))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*defun-inline-hook* 'defun-inline-hook))

(progn
  #+(or)
  (eval-when (:execute)
    (setq core:*echo-repl-read* t))
  
  #+(or)
  (defmacro debug-inline (msg &rest msg-args)
    `(progn
       (core:bformat t "debug-inline>> ")
       (core:bformat t ,msg ,@msg-args)
       (core:bformat t "%N")
       (finish-output)))
  (defmacro debug-inline (msg &rest msg-args)
    nil))

;;; This defines compiler macros that only come into effect when using cclasp.
;;; This is useful when their expansions involve cleavir-only special operators.
;;; Syntax is the same as define-compiler-macro, except that the lambda-list
;;; MUST start with (&whole something ...) for things to work.
;;; This macro is a little janky in that it doesn't work with declarations.
(defmacro define-cleavir-compiler-macro (name lambda-list &body body)
  `(define-compiler-macro ,name (,@lambda-list)
     ;; I just picked this since it's the first variable in auto-compile.lisp.
     (unless (eq cmp:*cleavir-compile-hook* 'cclasp-compile*)
       (return-from ,(core:function-block-name name) ,(second lambda-list)))
     ,@body))

;;; This stupid little macro is to tighten up
;;; (if (and (fixnump x) (>= x c1) (< x c2)) ...)
;;; which is useful for bounds checks.
;;; The compiler can't optimize this condition very well as I write this.
;;; NOTE: Evaluates VAL more than once.
(defmacro if-in-bounds ((val low high) then else)
  `(core::local-block nil
     (if (cleavir-primop:typeq ,val fixnum)
         (if (cleavir-primop:fixnum-not-less ,val ,low)
             (if (cleavir-primop:fixnum-less ,val ,high)
                 (return ,then)))
         (error 'type-error :datum ,val :expected-type 'fixnum))
     ,else))

(progn
  (debug-inline "eq")
  (declaim (inline cl:eq))
  (defun cl:eq (x y)
    (if (cleavir-primop:eq x y) t nil)))

(progn
  (debug-inline "eql")
  (declaim (inline cl:eql))
  (defun eql (x y)
    (cond ((cleavir-primop:eq x y) t)
          ((cleavir-primop:typeq x core::eq-incomparable)
           (if (cleavir-primop:typeq y core::eq-incomparable)
               (core:eql-underlying x y)
               nil))
          (t nil))))

#+(or)
(progn
  ;; Really want a bit-vector-equal intrinsic here.
  ;; Maybe even separate simple and not vectors.
  
  (defmacro type2case (x y fail &rest cases)
    (let ((sx (gensym "X")) (sy (gensym "Y")))
      `(let ((,sx ,x) (,sy ,y))
         (cond ,@(loop for (type . body) in cases
                       collect `((typep ,sx ',type)
                                 (if (typep ,sy ',type)
                                     (progn ,@body)
                                     ,fail))
                       collect `((typep ,sy ',type) ,fail))
               (t ,fail)))))
  
  (defun equal (x y)
    (or (eql x y)
        (type2case x y nil
                   (cons (and (equal (car x) (car y))
                              (equal (cdr x) (cdr y))))
                   (string (string= x y))
                   (bit-vector (bit-vector-equal x y))
                   (pathname (pathname-equal x y)))))
  
  (defun hash-table-equalp (x y)
    (and (eq (hash-table-count x) (hash-table-count y))
         (eq (hash-table-test x) (hash-table-test y))
         ;; since the number of entries is the same,
         ;; we don't need to check for extra keys in y.
         (maphash (lambda (k v)
                    (multiple-value-bind (otherv present)
                        (gethash k y)
                      (unless present
                        (return-from hash-table-equalp nil))
                      (unless (equalp v otherv)
                        (return-from hash-table-equalp nil))))
                  x)
         t))
  
  (defun equalp (x y)
    (or (eq x y)
        (type2case x y nil
                   (character (char-equal x y))
                   (number (= x y))
                   (cons (and (equalp (car x) (car y))
                              (equalp (cdr x) (cdr y))))
                   (array (array-equalp x y))
                   (structure-object (structure-equalp x y))
                   (hash-table (hash-table-equalp x y))
                   (pathname (pathname-equal x y)))))
  )

(progn
  (debug-inline "not")
  (declaim (inline cl:not))
  (defun not (object)
    (if (cleavir-primop:eq object nil) t nil)))

(progn
  (debug-inline "identity")
  (declaim (inline cl:identity))
  (defun identity (object)
    ;; preserve nontoplevelness
    (the t object)))

;;; Type predicates.
(macrolet ((defpred (name type)
             ;; We have to be careful about recursion - if one of these ended up
             ;; as a TYPEP call there could be disastrous recursion.
             ;; Here's a sanity check to make sure the type is something simple
             ;; enough to be done as a header check.
             ;; Since some aren't actually THAT simple but still won't be typep
             ;; it's not actually used, but, you know, it's there.
             #+(or)
             (unless (or (member type '(fixnum cons character single-float))
                         (gethash type core:+type-header-value-map+))
               (error "BUG: See comment in inline.lisp DEFPRED"))
             `(progn
                (debug-inline ,(symbol-name name))
                (declaim (inline ,name))
                (defun ,name (o)
                  (if (cleavir-primop:typeq o ,type) t nil))))
           (defpreds (&rest rest)
             `(progn
                ,@(loop for (fun name) on rest by #'cddr
                        collect `(defpred ,fun ,name)))))
  ;; Ideally, we want to cover standard type predicates, plus everything with a
  ;; core::simple-type-predicate,= as those will show up from TYPEP.

  ;; numbers are a bit weird cos of fixnums, but nonetheless
  ;; shouldn't revert to typep.
  ;; string is (or simple-base-string simple-character-string) so should be ok.
  ;; list is (or cons null) so should be ok.
  ;; atom is (not cons)
  (defpreds consp cons
    core:fixnump fixnum
    characterp character
    core:single-float-p single-float

    arrayp array
    atom atom
    complexp complex
    core:double-float-p double-float
    floatp float
    functionp function
    hash-table-p hash-table
    integerp integer
    listp list
    ;; null null ; defined with EQ below
    numberp number
    random-state-p random-state
    rationalp rational
    realp real
    packagep package
    pathnamep pathname
    core:data-vector-p core:abstract-simple-vector
    simple-array-p simple-array
    simple-bit-vector-p simple-bit-vector
    simple-string-p simple-string
    simple-vector-p simple-vector
    stringp string
    symbolp symbol
    vectorp vector)
  ;; standard predicates we can't define like this
  #+(or)
  (defpreds
    ;; standard-char-p standard-char ; not actually a type pred - only accepts chars
      ;; streamp stream ; no good as it's an extensible class... FIXME do it anyway?
      compiled-function-p compiled-function))

(progn
  (debug-inline "null")
  (declaim (inline cl:null))
  (defun cl:null (x)
    (eq x nil)))
;; (if (cleavir-primop:typeq x null) t nil)

(progn
  (debug-inline "endp")
  ;; more clhs-like definition is (null (the list x))
  (declaim (inline cl:endp))
  (defun cl:endp (list)
    (cond ((cleavir-primop:typeq list cons) nil) ; common case
          ((null list) t)
          (t (error 'type-error :datum list :expected-type 'list)))))

(progn
  (debug-inline "car")
  (declaim (inline cl:car))
  (defun cl:car (x)
    (if (cleavir-primop:typeq x cons)
        (cleavir-primop:car x)
        (if (eq x nil)
            nil
            (error 'type-error :datum x :expected-type 'list)))))

(progn
  (debug-inline "cdr")
  (declaim (inline cl:cdr))
  (defun cl:cdr (x)
    (if (cleavir-primop:typeq x cons) ;; (consp x)
        (cleavir-primop:cdr x)
        (if (null x)
            nil
            (error 'type-error :datum x :expected-type 'list)))))

(defmacro defcr (name &rest ops)
  `(progn
     (debug-inline ,(symbol-name name))
     (declaim (inline ,name))
     (defun ,name (x)
       ,(labels ((rec (ops)
                   (if (null ops)
                       'x
                       `(,(first ops) ,(rec (rest ops))))))
          (rec ops)))))

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

(debug-inline "rplaca")

(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))

(progn
  (declaim (inline cl:rplacd))
  (defun cl:rplacd (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplacd p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))


(debug-inline "primop")

(progn
  (defmacro define-with-contagion (inlined-name comparison (x y) fixnum single-float double-float generic)
    (declare (ignore comparison)) ; this will be used to control fp behavior, see CLHS 12.1.4.1
    `(progn
       (declaim (inline ,inlined-name))
       (defun ,inlined-name (,x ,y)
         (tagbody
            ;; FIXME: The "generic" jumps should actually coerce and then jump
            ;; to a specialized one.
            (cond ((cleavir-primop:typeq ,x fixnum)
                   (if (cleavir-primop:typeq ,y fixnum)
                       (go fixnum)
                       (go generic)))
                  ((cleavir-primop:typeq ,x single-float)
                   (cond ((cleavir-primop:typeq ,y single-float)
                          (go single-float))
                         #+(or)
                         ((cleavir-primop:typeq ,y double-float)
                          (setf ,x (cleavir-primop:coerce single-float double-float ,x))
                          (go double-float))
                         (t (go generic))))
                  ((cleavir-primop:typeq ,x double-float)
                   (cond ((cleavir-primop:typeq ,y double-float)
                          (go double-float))
                         #+(or)
                         ((cleavir-primop:typeq ,y single-float)
                          (setf ,y (cleavir-primop:coerce single-float double-float ,y))
                          (go double-float))
                         (t (go generic))))
                  (t (go generic)))
          fixnum (return-from ,inlined-name ,@fixnum)
          single-float (return-from ,inlined-name ,@single-float)
          double-float (return-from ,inlined-name ,@double-float)
          generic (return-from ,inlined-name ,@generic)))))
  (define-with-contagion primop:inlined-two-arg-+ nil (x y)
    ((cleavir-primop:let-uninitialized (z)
                                       (if (cleavir-primop:fixnum-add x y z)
                                           z
                                           (core:convert-overflow-result-to-bignum z))))
    ((cleavir-primop:float-add single-float x y))
    ((cleavir-primop:float-add double-float x y))
    ((core:two-arg-+ x y)))
  (define-with-contagion primop:inlined-two-arg-- nil (x y)
    ((cleavir-primop:let-uninitialized (z)
                                       (if (cleavir-primop:fixnum-sub x y z)
                                           z
                                           (core:convert-overflow-result-to-bignum z))))
    ((cleavir-primop:float-sub single-float x y))
    ((cleavir-primop:float-sub double-float x y))
    ((core:two-arg-- x y)))
  (define-with-contagion primop:inlined-two-arg-* nil (x y)
    ((go generic))             ; FIXME: fixnum arithmetic!
    ((cleavir-primop:float-mul single-float x y))
    ((cleavir-primop:float-mul double-float x y))
    ((core:two-arg-* x y)))
  (define-with-contagion primop:inlined-two-arg-/ nil (x y)
    ((go generic))             ; FIXME: fixnum arithmetic!
    ((cleavir-primop:float-div single-float x y))
    ((cleavir-primop:float-div double-float x y))
    ((core:two-arg-/ x y)))
  (defmacro defcomparison (inline-name fixnum-op float-op generic-name)
    `(define-with-contagion ,inline-name t (x y)
       ((if (,fixnum-op x y) t nil))
       ((if (,float-op single-float x y) t nil))
       ((if (,float-op double-float x y) t nil))
       ((,generic-name x y))))
  (defcomparison primop:inlined-two-arg-<
    cleavir-primop:fixnum-less        cleavir-primop:float-less        core:two-arg-<)
  (defcomparison primop:inlined-two-arg-<=
    cleavir-primop:fixnum-not-greater cleavir-primop:float-not-greater core:two-arg-<=)
  (defcomparison primop:inlined-two-arg-=
    cleavir-primop:fixnum-equal       cleavir-primop:float-equal       core:two-arg-=)
  (defcomparison primop:inlined-two-arg->
    cleavir-primop:fixnum-greater     cleavir-primop:float-greater     core:two-arg->)
  (defcomparison primop:inlined-two-arg->=
    cleavir-primop:fixnum-not-less    cleavir-primop:float-not-less    core:two-arg->=))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-cleavir-compiler-macro + (&whole form &rest numbers)
    (core:expand-associative '+ 'primop:inlined-two-arg-+ numbers 0))
  (define-cleavir-compiler-macro - (&whole form minuend &rest subtrahends)
    (if (core:proper-list-p subtrahends)
        (if subtrahends
            `(primop:inlined-two-arg-- ,minuend ,(core:expand-associative '+ 'primop:inlined-two-arg-+ subtrahends 0))
            `(core:negate ,minuend))
        (error "The - operator can not be part of a form that is a dotted list.")))
  (define-cleavir-compiler-macro * (&whole form &rest numbers)
    (core:expand-associative '* 'primop:inlined-two-arg-* numbers 1))
  (define-cleavir-compiler-macro / (&whole form dividend &rest divisors)
    (if (core:proper-list-p divisors)
        (if divisors
            `(primop:inlined-two-arg-/ ,dividend (* ,@divisors))
            `(primop:inlined-two-arg-/ 1 ,dividend))
        (error "The / operator can not be part of a form that is a dotted list.")))
  (define-cleavir-compiler-macro < (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-< numbers 'real))
  (define-cleavir-compiler-macro <= (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-<= numbers 'real))
  (define-cleavir-compiler-macro = (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-= numbers 'number))
  (define-cleavir-compiler-macro /= (&whole form &rest numbers)
    (core:expand-uncompare form 'primop:inlined-two-arg-= numbers 'number))
  (define-cleavir-compiler-macro > (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-> numbers 'real))
  (define-cleavir-compiler-macro >= (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg->= numbers 'real))
  (define-cleavir-compiler-macro 1+ (&whole form x)
    `(primop:inlined-two-arg-+ ,x 1))
  (define-cleavir-compiler-macro 1- (&whole form x)
    `(primop:inlined-two-arg-- ,x 1)))

(progn
  (debug-inline "plusp")
  (declaim (inline plusp))
  (defun plusp (real) (> real 0)))

(progn
  (debug-inline "minusp")
  (declaim (inline minusp))
  (defun minusp (number) (< number 0)))

;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;

(debug-inline "array-total-size")
(declaim (inline array-total-size))
(defun array-total-size (array)
  (etypecase array
    ((simple-array * (*)) (core::vector-length array))
    ;; MDArray
    (array (core::%array-total-size array))))

(debug-inline "array-rank")
(declaim (inline array-rank))
(defun array-rank (array)
  (etypecase array
    ((simple-array * (*)) 1)
    (array (core::%array-rank array))))

(debug-inline "array-dimension")
(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (if (zerop axis-number)
         (core::vector-length array)
         (error "Invalid axis number ~d for array of rank ~d" axis-number 1)))
    (array
     (if-in-bounds (axis-number 0 (core::%array-rank array))
                   (core::%array-dimension array axis-number)
                   (error "Invalid axis number ~d for array of rank ~d"
                          axis-number (core::%array-rank array))))))

;; Unsafe version for array-row-major-index
(debug-inline "%array-dimension")
(declaim (inline %array-dimension))
(defun %array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (core::vector-length array))
    (array (core::array-dimension array axis-number))))

(debug-inline "svref")
(declaim (inline svref))
(defun svref (vector index)
  (if (typep vector 'simple-vector)
      (let ((ats (core::vector-length vector)))
        (if-in-bounds (index 0 ats)
                      (cleavir-primop:aref vector index t t t)
                      (error "Invalid index ~d for vector of length ~d" index ats)))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

(declaim (inline (setf svref)))
(defun (setf svref) (value vector index)
  (if (typep vector 'simple-vector)
      (let ((ats (core::vector-length vector)))
        (if-in-bounds (index 0 ats)
                      (progn (cleavir-primop:aset vector index value t t t)
                             value)
                      (error "Invalid index ~d for vector of length ~d" index ats)))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

;;; Unsafe versions to use that don't check bounds (but still check type)
(debug-inline "svref/no-bounds-check")
(declaim (inline svref/no-bounds-check))
(defun svref/no-bounds-check (vector index)
  (if (typep vector 'simple-vector)
      (if (cleavir-primop:typeq index fixnum)
          (cleavir-primop:aref vector index t t t)
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

(declaim (inline (setf svref/no-bounds-check)))
(defun (setf svref/no-bounds-check) (value vector index)
  (if (typep vector 'simple-vector)
      (if (cleavir-primop:typeq index fixnum)
          (progn (cleavir-primop:aset vector index value t t t)
                 value)
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

(define-cleavir-compiler-macro svref (&whole whole vector index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(svref/no-bounds-check ,vector ,index)))
(define-cleavir-compiler-macro (setf svref)
    (&whole whole value vector index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(funcall #'(setf svref/no-bounds-check) ,value ,vector ,index)))

#+(or)
(progn
  (debug-inline "core:vref")
  (declaim (inline core:vref))
  (defun core:vref (array index)
    ;; FIXME: type inference should be able to remove the redundant
    ;; checking that it's an array... maybe?
    (macrolet ((mycase (&rest specs)
                 `(typecase array
                    ,@(loop for (type boxed) in specs
                            collect `((simple-array ,type (*))
                                      (cleavir-primop:aref array index ,type t ,boxed)))
                    (t
                     (core:bformat t "vref array-element-type: %s%N" (array-element-type array))
                     (error "BUG: vref unknown vector ~a" array)))))
      (mycase (t t) (base-char nil) (character nil)
              (double-float nil) (single-float nil)
              (fixnum nil)
              (ext:integer64 nil) (ext:integer32 nil)
              (ext:integer16 nil) (ext:integer8 nil)
              (ext:byte64 nil) (ext:byte32 nil)
              (ext:byte16 nil) (ext:byte8 nil)
              (bit t)))))

;;; This is unsafe in that it doesn't bounds check.
;;; It DOES check that the value is of the correct type,
;;; because this is the only place we know the type.
#+(or)
(progn
  (declaim (inline (setf core:vref)))
  (defun (setf core:vref) (value array index)
    (macrolet ((mycase (&rest specs)
                 `(typecase array
                    ,@(loop for (type boxed) in specs
                            collect `((simple-array ,type (*))
                                      (unless (typep value ',type)
                                        (error 'type-error :datum value :expected-type ',type))
                                      (cleavir-primop:aset array index value ,type t ,boxed)
                                      value))
                    ;; should be unreachable
                    (t (error "BUG: Unknown vector ~a" array)))))
      (mycase (t t) (base-char nil) (character nil)
              (double-float nil) (single-float nil)
              (fixnum nil)
              (ext:integer64 nil) (ext:integer32 nil)
              (ext:integer16 nil) (ext:integer8 nil)
              (ext:byte64 nil) (ext:byte32 nil)
              (ext:byte16 nil) (ext:byte8 nil)
              (bit t)))))

;;; Array indices are all fixnums. If we're sure sizes are valid, we don't want
;;; to use general arithmetic. We can just use this to do unsafe modular arithmetic.
;;; (Used in this file only)
(defmacro add-indices (a b)
  `(cleavir-primop:let-uninitialized (z)
     (if (cleavir-primop:fixnum-add ,a ,b z) z z)))

;; FIXME: Duplicate code from seqmacros.lsp.
(defmacro with-array-data ((arrayname offsetname array) &body body)
  `(multiple-value-bind (,arrayname ,offsetname)
       (let ((,arrayname ,array) (,offsetname 0))
         (loop
           (if (cleavir-primop:typeq ,arrayname core:abstract-simple-vector)
               (return (values ,arrayname ,offsetname)))
           (psetf ,arrayname (core::%displacement ,arrayname)
                  ,offsetname (add-indices
                               ,offsetname
                               (core::%displaced-index-offset ,arrayname)))))
     (declare (type (simple-array * (*)) ,arrayname)
              (type fixnum ,offsetname))
     ,@body))

(declaim (inline vector-read))
(defun vector-read (vector index)
  ;; first bounds check. Use the original arguments.
  ;; second, undisplace. This can be done independently
  ;; of the index, meaning it could potentially be
  ;; moved out of loops, though that can invite inconsistency
  ;; in a multithreaded environment.
  ;; NOTE: vector-length will be the fill pointer, if there is one.
  ;; ALSO NOTE: This function is only used in ELT. We know already
  ;; that vector really is a vector.
  (let ((max (core::vector-length vector)))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:sequence-out-of-bounds
                         :datum index :expected-type `(integer 0 (,max))
                         :object vector))
    (with-array-data (underlying-array offset vector)
      ;; Okay, now array is a vector/simple, and index is valid.
      ;; This function takes care of element type discrimination.
      (vref underlying-array (add-indices index offset)))))

(declaim (inline vector-set))
(defun vector-set (vector index value)
  ;; NOTE: This function is only used in CORE:SETF-ELT. We know already
  ;; that vector really is a vector.
  (let ((max (core::vector-length vector)))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:sequence-out-of-bounds
                         :datum index :expected-type `(integer 0 (,max))
                         :object vector))
    (with-array-data (underlying-array offset vector)
      ;; Okay, now array is a vector/simple, and index is valid.
      ;; This function takes care of element type discrimination.
      (setf (vref underlying-array (add-indices index offset)) value))))

(declaim (inline row-major-aref/no-bounds-check))
(defun row-major-aref/no-bounds-check (array index)
  ;; First, undisplace. This can be done independently
  ;; of the index, meaning it could potentially be
  ;; moved out of loops, though that can invite inconsistency
  ;; in a multithreaded environment.
  (with-array-data (underlying-array offset array)
    ;; Array is a vector/simple, and we assume index is valid.
    (core:vref underlying-array (add-indices index offset))))

(declaim (inline cl:row-major-aref))
(defun cl:row-major-aref (array index)
  (let ((max (etypecase array
               ((simple-array * (*)) (core::vector-length array))
               (array (core::%array-total-size array)))))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:row-major-out-of-bounds :datum index
                                                       :expected-type `(integer 0 (,max))
                                                       :object array)))
  (row-major-aref/no-bounds-check array index))
(define-cleavir-compiler-macro row-major-aref (&whole whole array index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(row-major-aref/no-bounds-check ,array ,index)))

(declaim (inline row-major-aset/no-bounds-check))
(defun row-major-aset/no-bounds-check (array index value)
  (with-array-data (underlying-array offset array)
    (setf (core:vref underlying-array (add-indices index offset)) value)))

(declaim (inline core:row-major-aset))
(defun core:row-major-aset (array index value)
  (let ((max (etypecase array
               ((simple-array * (*)) (core::vector-length array))
               (array (core::%array-total-size array)))))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:row-major-out-of-bounds :datum index
                                                       :expected-type `(integer 0 (,max))
                                                       :object array)))
  (row-major-aset/no-bounds-check array index value))
(define-cleavir-compiler-macro core:row-major-aset
    (&whole whole array index value &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(row-major-aset/no-bounds-check ,array ,index ,value)))

(declaim (inline schar (setf schar) char (setf char)))
(defun schar (string index)
  (row-major-aref (the simple-string string) index))
(defun (setf schar) (value string index)
  (core:row-major-aset (the simple-string string) index value))

(defun char (string index)
  (row-major-aref (the string string) index))
(defun (setf char) (value string index)
  (core:row-major-aset (the string string) index value))

(defun row-major-index-computer (array dimsyms subscripts)
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
          (the fixnum
               (+ ,@(loop for sub in subscripts
                          for subsym in (reverse subsyms)
                          collect `(* ,sub ,subsym)))))))))

;;; Insert some form if the policy is in effect, otherwise nil.
;;; intended use is like ,@(when-policy ...)
(defun when-policy (env policy form)
  (when (environment-has-policy-p env policy) (list form)))

;;; FIXME: core::%array-dimension won't work for simple vectors. Might need to
;;; shuffle type checks around to do things properly.
#+(or)
(define-cleavir-compiler-macro array-row-major-index
    (&whole form array &rest subscripts &environment env)
  ;; FIXME: Cleavir arithmetic is not yet clever enough for this to be fast in the
  ;; >1dimensional case. We need wrapped fixnum multiplication and addition, basically,
  ;; where overflow jumps to an error.
  ;; As such, we don't expand (using the C++ definition) for these cases.
  (if (> (length subscripts) 1)
      form
      (let* ((rank (length subscripts))
             (sarray (gensym "ARRAY"))
             (ssubscripts (loop repeat rank collecting (gensym "SUBSCRIPT")))
             (dimsyms (loop repeat rank collecting (gensym "DIMENSION")))
             (rmindex (gensym "ROW-MAJOR-INDEX")))
        ;; First up, once-only the array and subscripts.
        `(let ((,sarray ,array)
               ,@(loop for ssub in ssubscripts for sub in subscripts
                       collecting `(,ssub ,sub)))
           (declare (type fixnum ,@ssubscripts))
           ;; Now verify that the rank is correct (maybe)
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core:check-rank ,sarray ,rank))
           ;; We need the array dimensions, so bind those
           (let (,@(loop for dimsym in dimsyms
                         for axis below rank
                         collect `(,dimsym (core::%array-dimension ,sarray ,axis))))
             (declare (type fixnum ,@dimsyms))
             ;; Check that the index is valid (maybe)
             ,@(when (environment-has-policy-p env 'core::insert-array-bounds-checks)
                 (loop for ssub in ssubscripts
                       for dimsym in dimsyms
                       for axis below rank
                       collect `(core:check-index ,ssub ,dimsym ,axis)))
             ;; Now we know we're good, do the actual computation
             ,(row-major-index-computer sarray dimsyms ssubscripts))))))

(define-cleavir-compiler-macro aref (&whole form array &rest subscripts
                                            &environment env)
  ;; FIXME: See tragic comment above in array-row-major-index.
  (if (or (> (length subscripts) 1) (null subscripts))
      form
      (let ((sarray (gensym "ARRAY"))
            (index0 (gensym "INDEX0")))
        `(let ((,sarray ,array)
               (,index0 ,(first subscripts)))
           ,@(when-policy
              env 'cleavir-kildall-type-inference:insert-type-checks
              `(if (cleavir-primop:typeq ,sarray array)
                   nil
                   (error 'type-error :datum ,sarray :expected-type '(array * 1))))
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core::multiple-value-foreign-call
                "cm_check_index"
                ,index0
                (if (cleavir-primop:typeq ,sarray core:abstract-simple-vector)
                    (core::vector-length ,sarray)
                    (core::%array-dimension ,sarray 0))
                0))
           (with-array-data (data offset ,sarray)
             (core::MULTIPLE-VALUE-FOREIGN-CALL "cm_vref" data (add-indices offset ,index0)))))))

(define-cleavir-compiler-macro (setf aref) (&whole form new array &rest subscripts
                                                   &environment env)
  (if (or (> (length subscripts) 1) (null subscripts))
      form
      (let ((sarray (gensym "ARRAY"))
            (index0 (gensym "INDEX0")))
        `(let ((,sarray ,array)
               (,index0 ,(first subscripts)))
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core::multiple-value-foreign-call
                "cm_check_index"
                ,index0
                (if (cleavir-primop:typeq ,sarray core:abstract-simple-vector)
                    (core::vector-length ,sarray)
                    (core::%array-dimension ,sarray 0))
                0))
           (with-array-data (data offset ,sarray)
             (core::MULTIPLE-VALUE-FOREIGN-CALL "cm_vset" data (add-indices offset ,index0) ,new))))))

;;; ------------------------------------------------------------
;;;
;;; Sequence functions
;;;

(progn
  (debug-inline "length")
  (declaim (inline length))
  (defun length (sequence)
    (etypecase sequence
      (cons (core:cons-length sequence))
      ;; note: vector-length returns the fill pointer if there is one.
      (vector (core::vector-length sequence))
      (null 0))))

(progn
  (debug-inline "elt")
  (declaim (inline elt))
  (defun elt (sequence index)
    (etypecase sequence
      (cons
       (let ((cell (nthcdr index sequence)))
         (cond ((consp cell) (car (the cons cell)))
               ((null cell) ; Ran out of conses - index is too large.
                (error 'core:sequence-out-of-bounds
                       :datum index :object sequence
                       :expected-type `(integer 0 ,(1- (core:cons-length sequence)))))
               (t ; improper list.
                (error 'type-error :datum sequence :expected-type 'sequence)))))
      (vector (vector-read sequence index))
      (null (error 'core:sequence-out-of-bounds :datum index :expected-type '(integer 0 (0))
                                                :object sequence))))

  (debug-inline "core:setf-elt")
  (declaim (inline core:setf-elt))
  (defun core:setf-elt (sequence index new-value)
    (etypecase sequence
      (cons
       (let ((cell (nthcdr index sequence)))
         (cond ((consp cell) (setf (car (the cons cell)) new-value))
               ((null cell) ; Ran out of conses - index is too large.
                (error 'core:sequence-out-of-bounds
                       :datum index :object sequence
                       :expected-type `(integer 0 ,(1- (core:cons-length sequence)))))
               (t ; improper list.
                (error 'type-error :datum sequence :expected-type 'sequence)))))
      (vector (vector-set sequence index new-value))
      (null (error 'core:sequence-out-of-bounds :datum index :expected-type '(integer 0 (0))
                                                :object sequence)))))

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/assorted.lsp
;;;    and put here so that the inline definition is available
;;;
(declaim (inline core::coerce-fdesignator)
         (ftype (function ((or function symbol)) function)
                core::coerce-fdesignator))
(defun core::coerce-fdesignator (fdesignator)
  "Take a CL function designator and spit out a function."
  (etypecase fdesignator
    (function fdesignator)
    (symbol (fdefinition fdesignator))))

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/pprint.lsp
;;;    and put here so that the inline definition is available
;;;
(in-package "SI")

(declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (index-column (posn-index posn stream) stream))

(in-package #:clasp-cleavir)
;;; --------------------------------------------------
;;;
;;; Provided by bike  May 21, 2017
;;;

;;; should be moved to non-cleavir-specific land.
;;; Seems to be slower. Probably would be an improvement
;;; if the called function was inlined as well.
#+(or)
(progn
  (defun mapfoo-macro (iter accum function lists)
    ;; nothing cleavir-specific here
    (let ((sfunction (gensym "MAPCAR-FUNCTION"))
          (syms (loop repeat (length lists)
                      collect (gensym "MAPCAR-ARGUMENT"))))
      `(loop named ,(gensym "UNUSED-BLOCK")
             ;; the loop needs a (gensym) name so that ,function
             ;; can't refer to it.
             with ,sfunction = ,function
             ,@(loop for sym in syms
                     for list in lists
                     append `(for ,sym ,iter ,list))
             ,accum (funcall ,sfunction ,@syms))))

  (define-compiler-macro mapc (function list &rest more-lists)
    (mapfoo-macro 'in 'do function (cons list more-lists)))
  (define-compiler-macro mapcar (function list &rest more-lists)
    (mapfoo-macro 'in 'collect function (cons list more-lists)))
  (define-compiler-macro mapcan (function list &rest more-lists)
    (mapfoo-macro 'in 'nconc function (cons list more-lists)))
  (define-compiler-macro mapl (function list &rest more-lists)
    (mapfoo-macro 'on 'do function (cons list more-lists)))
  (define-compiler-macro maplist (function list &rest more-lists)
    (mapfoo-macro 'on 'collect function (cons list more-lists)))
  (define-compiler-macro mapcon (function list &rest more-lists)
    (mapfoo-macro 'on 'nconc function (cons list more-lists)))
  )

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
        (let* ((func-info (cleavir-env:function-info env name))
               (notinline (and func-info
                               (eq 'notinline (cleavir-env:inline func-info))))
               ;; We can't get this from the func-info because it might be
               ;; a local-function-info, which doesn't have that slot.
               (cmf (compiler-macro-function name env)))
          (if (and cmf (not notinline))
              (funcall *macroexpand-hook* cmf form env)
              `(cleavir-primop:funcall ,function ,@arguments))))))
  `(cleavir-primop:funcall
    (core::coerce-fdesignator ,function)
    ,@arguments))

(define-cleavir-compiler-macro values (&whole form &rest values)
  `(cleavir-primop:values ,@values))

;;; Written as a compiler macro to avoid confusing bclasp.
(define-cleavir-compiler-macro multiple-value-bind (&whole form vars values-form &body body)
  (let ((syms (loop for var in vars collecting (gensym (symbol-name var)))))
    `(cleavir-primop:let-uninitialized (,@syms)
       (cleavir-primop:multiple-value-setq (,@syms) ,values-form)
       (let (,@(loop for var in vars for sym in syms
                     collecting `(,var ,sym)))
         ,@body))))

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

;;; I'm not sure I understand the order of evaluation issues entirely,
;;; so I'm antsy about using the m-v-setq primop directly... and this
;;; equivalence is guaranteed.
;;; SETF VALUES will expand into a multiple-value-bind, which will use
;;; the m-v-setq primop as above, so it works out about the same.
;;; Not a cleavir macro because all we need is setf.
(define-compiler-macro multiple-value-setq ((&rest vars) form)
  ;; SETF VALUES will return no values if it sets none, but m-v-setq
  ;; always returns the primary value.
  (if (null vars)
      `(values ,form)
      `(values (setf (values ,@vars) ,form))))
