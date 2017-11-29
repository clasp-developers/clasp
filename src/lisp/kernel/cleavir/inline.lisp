(in-package :clasp-cleavir)

(eval-when (:execute)
  (setq core:*echo-repl-read* t))

#+(or)
(defmacro debug-inline (msg &rest msg-args)
  `(progn
     (format t "debug-inline>> ")
     (format t ,msg ,@msg-args)
     (format t "~%")
     (finish-output)))
(defmacro debug-inline (msg &rest msg-args)
  nil)

(progn
  (debug-inline "eq")
  (declaim (inline cl:eq))
  (defun cl:eq (x y)
    (if (cleavir-primop:eq x y) t nil)))

;;; FIXME: it would be nicer if this was just an inline definition
;;; referring to core:eql or something.
(progn
  (deftype eq-incomparable () '(and number (not fixnum) (not single-float)))
  (defun eql (x y)
    (cond ((eq x y) t)
          ((cleavir-primop:typeq x eq-incomparable)
           (if (cleavir-primop:typeq y eq-incomparable)
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
#+(or) ; recursion problem: e.g. (streamp x) => (typeq x stream) => (typep x 'stream) => (streamp x)
(macrolet ((defpred (name type)
             `(progn
                (debug-inline ,(symbol-name name))
                (declaim (inline ,name))
                (defun ,name (o)
                  (if (cleavir-primop:typeq o ,type) t nil))))
           (defpreds (&rest rest)
             `(progn
                ,@(loop for (fun name) on rest by #'cddr
                        collect `(defpred ,fun ,name)))))
  (defpreds consp cons
    hash-table-p hash-table
    simple-string-p simple-string
    atom atom
    simple-vector-p simple-vector
    integerp integer
    numberp number
    random-state-p random-state
    compiled-function-p compiled-function
    standard-char-p standard-char
    simple-bit-vector-p simple-bit-vector
    realp real
    stringp string
    functionp function
    streamp stream
    floatp float
    symbolp symbol
    pathnamep pathname
    arrayp array
    vectorp vector
    characterp character
    packagep package
    listp list
    complexp complex
    rationalp rational))

;;; On BOEHMDC, we don't have primitive type information, so typeq
;;; just defers to typep anyway.
#-use-boehmdc
(define-compiler-macro typep
    (&whole whole object type &optional env &environment macro-env)
  (if (and (null env) (constantp type macro-env))
      `(if (cleavir-primop:typeq ,object ,(ext:constant-form-value type macro-env))
           t nil)
      whole))

#-use-boehmdc
(progn
  (debug-inline "consp")
  (declaim (inline cl:consp))
  (defun cl:consp (x)
    (if (cleavir-primop:typeq x cons) t nil)))

#-use-boehmdc
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

#-use-boehmdc
(progn
  (debug-inline "fixnump")
  (declaim (inline core:fixnump))
  (defun core:fixnump (x)
    (if (cleavir-primop:typeq x cl:fixnum) t nil)))

#+(or)
(progn
  (declaim (inline cl:characterp))
  (defun cl:characterp (x)
    (if (cleavir-primop:typeq x cl:character) t nil)))

#+(or)
(progn
  (declaim (inline core:single-float-p))
  (defun core:single-float-p (x)
    (if (cleavir-primop:typeq x cl:single-float) t nil)))

#-use-boehmdc
(progn
  (debug-inline "car")
  (declaim (inline cl:car))
  (defun cl:car (x)
    (if (consp x)
        (cleavir-primop:car x)
        (if (null x)
            nil
            (error 'type-error :datum x :expected-type 'list)))))

#-use-boehmdc
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

#-use-boehmdc
(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))

#-use-boehmdc
(progn
  (declaim (inline cl:rplacd))
  (defun cl:rplacd (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplacd p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))
                                        

(debug-inline "primop")

(defpackage "PRIMOP"
  (:export #:convert-to-bignum
           #:inlined-two-arg-+
           #:inlined-two-arg--
           #:inlined-two-arg-*
           #:inlined-two-arg-/
           #:inlined-two-arg-<
           #:inlined-two-arg-<=
           #:inlined-two-arg-=
           #:inlined-two-arg->
           #:inlined-two-arg->=
           ))

#-use-boehmdc
(progn
  (defun convert-to-bignum (z)
    (if (> z 0)
        (- z (expt 2 cl-fixnum-bits))
        (+ z (expt 2 cl-fixnum-bits))))
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
    ((go generic)) ; FIXME: fixnum arithmetic!
    ((cleavir-primop:float-mul single-float x y))
    ((cleavir-primop:float-mul double-float x y))
    ((core:two-arg-* x y)))
  (define-with-contagion primop:inlined-two-arg-/ nil (x y)
    ((go generic)) ; FIXME: fixnum arithmetic!
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

#-use-boehmdc
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-compiler-macro + (&rest numbers)
    (core:expand-associative '+ 'primop:inlined-two-arg-+ numbers 0))
  (define-compiler-macro - (minuend &rest subtrahends)
    (if (core:proper-list-p subtrahends)
        (if subtrahends
            `(primop:inlined-two-arg-- ,minuend ,(core:expand-associative '+ 'primop:inlined-two-arg-+ subtrahends 0))
            `(core:negate ,minuend))
        (error "The - operator can not be part of a form that is a dotted list.")))
  (define-compiler-macro * (&rest numbers)
    (core:expand-associative '* 'primop:inlined-two-arg-* numbers 1))
  (define-compiler-macro / (dividend &rest divisors)
    (if (core:proper-list-p divisors)
        (if divisors
            `(primop:inlined-two-arg-/ ,dividend (* ,@divisors))
            `(primop:inlined-two-arg-/ 1 ,dividend))
        (error "The / operator can not be part of a form that is a dotted list.")))
  (define-compiler-macro < (&rest numbers)
    (core:expand-compare 'primop:inlined-two-arg-< numbers))
  (define-compiler-macro <= (&rest numbers)
    (core:expand-compare 'primop:inlined-two-arg-<= numbers))
  (define-compiler-macro = (&rest numbers)
    (core:expand-compare 'primop:inlined-two-arg-= numbers))
  (define-compiler-macro > (&rest numbers)
    (core:expand-compare 'primop:inlined-two-arg-> numbers))
  (define-compiler-macro >= (&rest numbers)
    (core:expand-compare 'primop:inlined-two-arg->= numbers))
  (define-compiler-macro 1+ (x)
    `(primop:inlined-two-arg-+ ,x 1))
  (define-compiler-macro 1- (x)
    `(primop:inlined-two-arg-- ,x 1)))

(progn
  (debug-inline "plusp")
  (declaim (inline plusp))
  (defun plusp (number)
    (> number 0)))

(progn
  (debug-inline "minusp")
  (declaim (inline minusp))
  (defun minusp (number)
    (< number 0)))


;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;

(declaim (inline array-total-size))
(defun array-total-size (array)
  (etypecase array
    ((simple-array * (*)) (core::vector-length array))
    ;; MDArray
    (array (core::%array-total-size array))))

(declaim (inline array-rank))
(defun array-rank (array)
  (etypecase array
    ((simple-array * (*)) 1)
    (array (core::%array-rank array))))

(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (if (zerop axis-number)
         (core::vector-length array)
         (error "Invalid axis number ~d for array of rank ~d" axis-number 1)))
    (array
     (if (and (>= axis-number 0) (< axis-number (core::%array-rank array)))
         (core::%array-dimension array axis-number)
         (error "Invalid axis number ~d for array of rank ~d" axis-number (core::%array-rank array))))))

;; Unsafe version for array-row-major-index
(declaim (inline %array-dimension))
(defun %array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (core::vector-length array))
    (array (core::array-dimension array axis-number))))

(declaim (inline svref))
(defun svref (vector index)
  (if (typep vector 'simple-vector)
      (if (typep index 'fixnum)
          (let ((ats (core::vector-length vector)))
            (if (and (<= 0 index) (< index ats))
                (cleavir-primop:aref vector index t t t)
                (error "Invalid index ~d for vector of length ~d" index ats)))
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

;;; (setf (svref x y) z) macroexpands into (core:setf-svref x y z)
(declaim (inline core:setf-svref))
(defun core:setf-svref (vector index value)
  (if (typep vector 'simple-vector)
      (if (typep index 'fixnum)
          (let ((ats (core::vector-length vector)))
            (if (and (<= 0 index) (< index ats))
                (progn (cleavir-primop:aset vector index value t t t)
                       value)
                (error "Invalid index ~d for vector of length ~d" index ats)))
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))

(declaim (inline %unsafe-vector-ref))
(defun %unsafe-vector-ref (array index)
  ;; FIXME: type inference should be able to remove the redundant
  ;; checking that it's an array... maybe?
  (macrolet ((mycase (&rest specs)
               `(typecase array
                  ,@(loop for (type boxed) in specs
                          collect `((simple-array ,type (*))
                                    (cleavir-primop:aref array index ,type t ,boxed)))
                  (t (error "BUG: Unknown vector ~a" array)))))
    (mycase (t t) (base-char nil) (character nil)
            (double-float nil) (single-float nil)
            (fixnum nil)
            (ext:integer64 nil) (ext:integer32 nil)
            (ext:integer16 nil) (ext:integer8 nil)
            (ext:byte64 nil) (ext:byte32 nil)
            (ext:byte16 nil) (ext:byte8 nil)
            (bit t))))

;;; This is "unsafe" in that it doesn't bounds check.
;;; It DOES check that the value is of the correct type,
;;; because this is the only place we know the type.
(declaim (inline %unsafe-vector-set))
(defun %unsafe-vector-set (array index value)
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
            (bit t))))

(declaim (inline row-major-array-in-bounds-p))
(defun row-major-array-in-bounds-p (array index)
  (etypecase array
    ((simple-array * (*))
     (and (<= 0 index) (< index (core::vector-length array))))
    (array
     (and (<= 0 index) (< index (core::%array-total-size array))))))

;; FIXME: This could be a function returning two values. But that's
;; quite inefficient at the moment.
(defmacro with-array-data ((arrayname offsetname array) &body body)
  `(let ((,arrayname ,array) (,offsetname 0))
     (declare (type fixnum ,offsetname))
     (etypecase ,arrayname
       ((simple-array * (*))) ; already all set
       (simple-array ; multidimensional. guaranteed to have offset zero and no recursion.
        (setf ,arrayname (core::%displacement ,arrayname)))
       (array
        (loop
          (let ((displacement (core::%displacement ,arrayname))
                (displaced-index-offset (core::%displaced-index-offset ,arrayname)))
            (setf underlying-array displacement
                  ,offsetname (+ ,offsetname displaced-index-offset)))
          (when (typep ,arrayname '(simple-array * (*))) (return)))))
     ,@body))

(declaim (inline cl:row-major-aref))
(defun cl:row-major-aref (array index)
  ;; First, undisplace. This can be done independently
  ;; of the index, meaning it could potentially be
  ;; moved out of loops, though that can invite inconsistency
  ;; in a multithreaded environment.
  (with-array-data (underlying-array offset array)
    ;; Now bounds check. Use the original arguments.
    (unless (row-major-array-in-bounds-p array index)
      (error "~d is not a valid row-major index for ~a" index array))
    ;; Okay, now array is a vector/simple, and index is valid.
    ;; This function takes care of element type discrimination.
    (%unsafe-vector-ref underlying-array (+ index offset))))

(declaim (inline core:row-major-aset))
(defun core:row-major-aset (array index value)
  (with-array-data (underlying-array offset array)
    (unless (row-major-array-in-bounds-p array index)
      (error "~d is not a valid row-major index for ~a" index array))
    (%unsafe-vector-set underlying-array (+ index offset) value)))


(declaim (inline schar core:schar-set char core:char-set))
(defun schar (string index)
  (row-major-aref (the simple-string string) index))
(defun core:schar-set (string index value)
  (core:row-major-aset (the simple-string string) index value))

(defun char (string index)
  (row-major-aref (the string string) index))
(defun core:char-set (string index value)
  (core:row-major-aset (the string string) index value))

(defun row-major-index-computer (array dimsyms subscripts)
  ;; assumes once-only is taken care of
  (let ((dimsyms (loop for sub in (rest subscripts) collect (gensym "DIM")))
        (subsyms (loop for sub in subscripts collect (gensym "SUB"))))
    (if subscripts
        `(let* ((,(first subsyms) 1)
                ,@(loop for dimsym in (reverse dimsyms)
                        for subsym in (cdr subsyms)
                        for lastsym in subsyms
                        collect `(,subsym (* ,lastsym ,dimsym))))
           (declare (type fixnum ,@subsyms))
           (the fixnum
                (+ ,@(loop for sub in subscripts
                           for subsym in (reverse subsyms)
                           collect `(* ,sub ,subsym)))))
        '0)))

(define-compiler-macro array-row-major-index (array &rest subscripts)
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
       ;; Now verify that the rank is correct
       (unless (eq (array-rank ,sarray) ,rank)
         (error "Wrong number of subscripts, ~d, for an array of rank ~d."
                ,rank (array-rank ,sarray)))
       ;; We need the array dimensions, so bind those
       (let (,@(loop for dimsym in dimsyms
                     for axis below rank
                     collect `(dimsym (%array-dimension ,sarray ,axis))))
         (declare (type fixnum ,@dimsyms))
         ;; Check that the index is valid
         ,@(loop for ssub in ssubscripts
                 for dimsym in dimsyms
                 for axis below rank
                 collect `(unless (and (>= ,ssub 0) (< ,ssub ,dimsym))
                            (error "Invalid index ~d for axis ~d of array: expected 0-~d"
                                   ,ssub ,axis ,dimsym)))
         ;; Now we know we're good, do the actual computation
         ,(row-major-index-computer sarray dimsyms ssubscripts)))))

#+(or)
(define-compiler-macro %aref (array &rest subscripts)
  (case (length subscripts)
    ((1) `(%row-major-aref ,array ,(first subscripts)))
    (t (let ((asym (gensym "ARRAY")))
         `(let ((,asym ,array))
            (%row-major-aref ,asym (array-row-major-index ,asym ,@subscripts)))))))

;;; ------------------------------------------------------------
;;;
;;; Sequence functions
;;;

(declaim (inline length))
(defun length (sequence)
  (etypecase sequence
    (cons
     (locally
         (declare ;(optimize speed)
                  (type cons sequence))
       (let ((length 1))
         (loop (let ((next (cdr sequence)))
                 (etypecase next
                   (cons (setf sequence next length (1+ length)))
                   (null (return-from length length))))))))
    (vector (core::vector-length sequence))
    (null 0)))

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

(define-compiler-macro funcall (&whole form function &rest arguments &environment env)
  ;; If we have (funcall #'foo ...), we might be able to apply the FOO compiler macro.
  (when (and (consp function) (eq (first function) 'function)
             (consp (cdr function)) (null (cddr function)))
    (let ((cmf (compiler-macro-function (second function) env)))
      (when cmf
        (return-from funcall
          ;; incidentally, this funcall should be okay given that it's compile-time.
          (funcall cmf form env)))))
  ;; If not, we can stil eliminate the call to FUNCALL as follows.
  (let ((fsym (gensym "FUNCTION")))
    `(let ((,fsym ,function))
       (cleavir-primop:funcall
        (cond
          ((cleavir-primop:typeq ,fsym function)
           ,fsym)
          ((cleavir-primop:typeq ,fsym symbol)
           (symbol-function ,fsym))
          (t (error 'type-error :datum ,fsym :expected-type '(or symbol function))))
        ,@arguments))))

;;; FIXME:  This relies on ir.lisp: return-value-elt to work properly and it
;;;         isn't completely implemented - it needs a GEP instruction.
(define-compiler-macro values (&rest values)
  `(cleavir-primop:values ,@values))

#++
(defun symbol-value (symbol)
  (if (cleavir-primop:typeq symbol symbol)
      (cleavir-primop:symbol-value symbol)
      (error 'type-error :datum symbol :expected-type 'symbol)))
