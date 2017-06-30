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


;;; Use typeq
(progn
  (declaim (inline cl:consp))
  (defun cl:consp (x)
    (if (cleavir-primop:typeq x cons) t nil)))

(debug-inline "null")

(progn
  (declaim (inline cl:null))
  (defun cl:null (x)
    (eq x nil)))
;; if (cleavir-primop:typeq x null) t nil)))

(debug-inline "fixnump")

(progn
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

(debug-inline "car")

(progn
  (declaim (inline cl:car))
  (defun cl:car (x)
    (if (consp x)
        (cleavir-primop:car x)
        (if (null x)
            nil
            (error "Cannot get car of non-list ~s of type ~a" x (type-of x))))))

(debug-inline "cdr")

(progn
  (declaim (inline cl:cdr))
  (defun cl:cdr (x)
    (if (cleavir-primop:typeq x cons) ;; (consp x)
        (cleavir-primop:cdr x)
        (if (null x)
            nil
            (error "Cannot get cdr of non-list ~s" x)))))

(debug-inline "rplaca")

(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error "Cannot rplaca non-cons ~s" p))))

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


;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;

#+(or)
(progn

;;; This expands into a cond over all the array element types.
;;; The body of each cond is a macro form where the macro is local,
;;; and defined as (name (,uaet ,boxp) ,@body).
;;; It's kind of confusing but I think it's ok here.
(defmacro uaet-typeqcase (array (uaet boxp) &body body)
  `(macrolet ((bodyer (,uaet ,boxp) ,@body))
     (cond
       ,@(loop for uaet in sys::+upgraded-array-element-types+
               for boxp in +uaet-boxed+
               collect `((cleavir-primop:typeq ,array (array ,uaet))
                         (bodyer ,uaet ,boxp)))
       (t (error "BUG: Missing UAET information for AREF")))))

(defun row-major-aref (array index)
  ;; We could inline accesses to a non-simple array by looping to get the
  ;; underlying simple vector. But it might not be worth the complexity.
  (if (cleavir-primop:typeq array simple-array)
      (let ((data (if (cleavir-primop:typeq array vector)
                      array
                      (primop:array-displacement array))))
        (uaet-typeqcase data (uaet boxp)
          `(cleavir-primop:aref data index ,uaet t ,boxp))
      (locally (declare (notinline row-major-aref))
        (row-major-aref array index)))))

;;; setf row-major-aref primitively expands into core:row-major-aset.
;;; I prefer a new setf definition to redefining or layering over core functions.
(defsetf row-major-aref (array index) (value)
  `(if (cleavir-primop:typeq ,array simple-array)
       (let ((data (if (cleavir-primop:typeq ,array vector)
                       ,array
                       (primop:array-underlying ,array))))
         ;; Yow. Sorry. Try macroexpanding if you don't get it.
         (uaet-typeqcase data (uaet boxp)
           `(cleavir-primop:aset data ,',index ,',value ,uaet t ,boxp)))
       (core:row-major-aset ,array ,index ,value)))

)

#+(or)
(progn

(declaim (inline svref)
         (ftype (function (simple-vector fixnum) (values t &rest nil)) svref))
(defun svref (simple-vector index)
  ;; the type declaration should take care of the type check, but
  (if (array-in-bounds-p simple-vector index)
      (cleavir-primop:aref simple-vector index t t t)
      (error "Out of bounds: (svref ~s ~s)" simple-vector index)))

;;; Primitively, setf svref expands into core:setf-svref.
;;; Ditto above comment on setf.
(defsetf svref (simple-vector index) (value)
  ;; I don't think anything else will put in the type declaration, therefore
  `(progn
     (cond ((array-in-bounds-p ,simple-vector ,index)
            (cleavir-primop:aset (the simple-vector ,simple-vector)
                                 ,index ,value t t t)
            ,value)
           (t (error "Out of bounds: (setf (svref ~s ~s) ~s)"
                     ,simple-vector ,index ,value)))))

)

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/assorted.lsp
;;;    and put here so that the inline definition is available
;;;
(declaim (inline coerce-fdesignator)
	 (ftype (function ((or function symbol)) function) fdesignator))
(defun coerce-fdesignator (fdesignator)
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

#++
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
  (define-compiler-macro mapcan (function list &rest more-lists)
    (mapfoo-macro 'on 'collect function (cons list more-lists)))
  (define-compiler-macro mapcan (function list &rest more-lists)
    (mapfoo-macro 'on 'nconc function (cons list more-lists)))
  )

#++
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
