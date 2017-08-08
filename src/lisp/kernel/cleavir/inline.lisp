(in-package :clasp-cleavir)

(eval-when (:execute)
  (setq core:*echo-repl-tpl-read* t))

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
#+(or)
(progn
  (deftype eq-incomparable () '(and number (not fixnum)))
  
  (define-compiler-macro eql (x y)
    (let ((sx (gensym "EQL-X"))
          (sy (gensym "EQL-Y")))
      `(let ((,sx ,x) (,sy ,y))
         (cond ((eq ,sx ,sy) t)
               ((cleavir-primop:typeq ,sx eq-incomparable)
                (if (cleavir-primop:typeq ,sy eq-incomparable)
                    (locally (declare (notinline eql)) ; avoid recursive c-m expansion
                      (eql ,sx ,sy))
                    nil))
               (t nil))))))

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
            (error "Cannot get car of non-list ~s of type ~a" x (type-of x))))))

#-use-boehmdc
(progn
  (debug-inline "cdr")
  (declaim (inline cl:cdr))
  (defun cl:cdr (x)
    (if (cleavir-primop:typeq x cons) ;; (consp x)
        (cleavir-primop:cdr x)
        (if (null x)
            nil
            (error "Cannot get cdr of non-list ~s" x)))))

;;; FIXME: This takes a lot of itme to compile for some reason?
#+(or)
(progn

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
)

(debug-inline "rplaca")

#-use-boehmdc
(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error "Cannot rplaca non-cons ~s" p))))

#-use-boehmdc
(progn
  (declaim (inline cl:rplacd))
  (defun cl:rplacd (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplacd p v)
          p)
        (error "Cannot rplacd non-cons ~s" p))))


(debug-inline "primop")

(defpackage "PRIMOP"
  (:export #:convert-to-bignum
           #:inlined-two-arg-+
           #:inlined-two-arg--
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
  (defmacro def-inline-arithmetic (inlined-name cleavir-primop generic-name)
    (let ((x (gensym))
          (y (gensym))
          (z (gensym)))
      `(progn
         (declaim (inline ,inlined-name))
         (defun ,inlined-name (,x ,y)
           (block nil
             (tagbody
                (if (cleavir-primop:typeq ,x fixnum)
                    (if (cleavir-primop:typeq ,y fixnum)
                        (cleavir-primop:let-uninitialized (,z)
                                                          (if (,cleavir-primop ,x ,y ,z)
                                                              (return ,z)
                                                              (return (core:convert-overflow-result-to-bignum ,z))))
                        (go generic))
                    (go generic))
                ;; ... Other tests
              generic
                (return (,generic-name ,x ,y))))))))
  (def-inline-arithmetic primop:inlined-two-arg-+ cleavir-primop:fixnum-+ core:two-arg-+)
  (def-inline-arithmetic primop:inlined-two-arg-- cleavir-primop:fixnum-- core:two-arg--)
  ;;; Need * / and other primops
  (defmacro def-inline-comparison (inlined-name cleavir-primop generic-name)
    (let ((x (gensym))
          (y (gensym)))
      `(progn
         (declaim (inline ,inlined-name))
         (defun ,inlined-name (,x ,y)
           (block nil
             (tagbody
                (if (cleavir-primop:typeq ,x fixnum)
                    (if (cleavir-primop:typeq ,y fixnum)
                        (if (,cleavir-primop ,x ,y)
                            (return t)
                            (return nil)))
                    (go generic))
                ;; ... Other tests
              generic
                (return (,generic-name ,x ,y))))))))
  (def-inline-comparison primop:inlined-two-arg-<  cleavir-primop:fixnum-<  core:two-arg-<)
  (def-inline-comparison primop:inlined-two-arg-<= cleavir-primop:fixnum-<= core:two-arg-<=)
  (def-inline-comparison primop:inlined-two-arg-=  cleavir-primop:fixnum-=  core:two-arg-=)
  (def-inline-comparison primop:inlined-two-arg->  cleavir-primop:fixnum->  core:two-arg->)
  (def-inline-comparison primop:inlined-two-arg->= cleavir-primop:fixnum->= core:two-arg->=))

#-use-boehmdc
(progn
  (define-compiler-macro + (&rest numbers)
    (core:expand-associative '+ 'primop:inlined-two-arg-+ numbers 0))
#+(or)(define-compiler-macro - (&rest numbers)
        (core:expand-associative '- 'primop:inlined-two-arg-- numbers 0))
(define-compiler-macro - (minuend &rest subtrahends)
  (if (core:proper-list-p subtrahends)
      (if subtrahends
          `(primop:inlined-two-arg-- ,minuend ,(core:expand-associative '+ 'primop:inlined-two-arg-+ subtrahends 0))
          `(core:negate ,minuend))
      (error "The - operator can not be part of a form that is a dotted list.")))

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
#+(or)
(progn
(define-compiler-macro aref (array &rest indices)
  (let ((arr (gensym "ARRAY")))
    `(let ((,arr ,array))
       (row-major-aref ,array (array-row-major-index ,array ,@indices)))))

(define-compiler-macro array-row-major-index (array &rest subscripts)
  (let* ((rank (length subscripts))
         (dimsyms (loop repeat rank collect (gensym "DIM")))
         (subsyms (loop repeat rank collect (gensym "SUB")))
         (a (gensym "ARRAY")))
    `(let ((,a ,array))
       (unless (typep array '(array * ,rank))
         (error 'type-error :datum ,a :expected-type '(array * ,rank)))
       ,(if subscripts
            `(let (,@(loop for dimsym in dimsyms
                           for d from 0
                           collect `(,dimsym (array-dimension ,a ,d))))
               (let* ((,(first subsyms) 1)
                      ,@(loop for dimsym in (reverse (cdr dimsyms))
                              for subsym in (cdr subsyms)
                              for lastsym in subsyms
                              collect `(,subsym (* ,lastsym ,dimsym))))
                 (+ ,@(loop for sub in subscripts
                            for subsym in (reverse subsyms)
                            collect `(* ,sub ,subsym)))))
            0))))

  (declaim (inline svref))
  (defun svref (vector index)
    (cleavir-primop:aref vector index t t t))
  )



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

#-use-boehmdc
(define-compiler-macro funcall (function &rest arguments)
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
