(in-package #:clasp-cleavir)

(defun kwarg-presence (keyword required optional rest sys)
  (let ((result nil) (oddreq nil)
        (mems (list keyword)) (ktype (ctype:member sys keyword)))
    (loop for (param . r) on required by #'cddr
          ;; Check if it's definitely the keyword.
          when (and (ctype:member-p sys param)
                    (equal (ctype:member-members sys param) mems))
            do (return-from kwarg-presence t)
          ;; Check if it might be the keyword, if we haven't determined
          ;; this already.
          when (and (null result)
                    (ctype:subtypep ktype param sys))
            do (setf result :maybe)
          ;; Check for an odd number of required parameters.
          when (null r)
            do (setf oddreq t))
    ;; Now do the same thing with optionals, if we haven't got a maybe.
    ;; We don't need to check equality since optional means these might
    ;; not even be passed.
    (when (eq result :maybe) (return-from kwarg-presence result))
    (loop for (param) on (if oddreq (cdr optional) optional) by #'cddr
          when (ctype:subtypep ktype param sys)
            do (return-from kwarg-presence :maybe))
    ;; Finally the rest.
    (if (ctype:subtypep ktype rest sys)
        :maybe
        nil)))

(defun kwarg-type (keyword required optional rest sys)
  (let ((result (ctype:bottom sys)) (oddreq nil) (ambiguousp nil)
        (mems (list keyword)) (ktype (ctype:member sys keyword)))
    (loop for (param . r) on required by #'cddr
          ;; If this is the first param we've seen that could be the keyword,
          ;; and it must be the keyword, we're done.
          when (and (not ambiguousp)
                    (ctype:member-p sys param)
                    (equal (ctype:member-members sys param) mems))
            do (return-from kwarg-type (cond (r (first r))
                                             (optional (first optional))
                                             (t rest)))
          when (ctype:subtypep ktype param sys)
            do (setf result (ctype:disjoin sys result
                                           (cond (r (first r))
                                                 (optional (first optional))
                                                 (t rest)))
                     ;; Mark that we've seen the possible keyword, so that
                     ;; future iterations don't hit the short circuit above.
                     ambiguousp t)
          when (null r)
            do (setf oddreq t))
    (loop for (param . r) on (if oddreq (cdr optional) optional) by #'cddr
          when (ctype:subtypep ktype param sys)
            do (setf result (ctype:disjoin sys result
                                           (if r (first r) rest))))
    (if (ctype:subtypep ktype rest sys)
        (ctype:disjoin sys result rest)
        result)))

;;; Determines the real type of a keyword argument given its default type and the
;;; result of kwarg-presence. For example if the keyword only may be present, the
;;; default type is stuck in there.
(defun defaulted-kwarg-type (keyword presence default required optional rest sys)
  (ecase presence
    ((nil) default)
    ((t) (kwarg-type keyword required optional rest sys))
    ((:maybe)
     (ctype:disjoin sys default (kwarg-type keyword required optional rest sys)))))

;;;

(defmacro with-types (lambda-list argstype (&key default) &body body)
  (multiple-value-bind (req opt rest keyp keys)
      (core:process-lambda-list lambda-list 'function)
    ;; We ignore &allow-other-keys because it's too much effort for
    ;; type derivation. Approximate results are fine.
    (if (and (zerop (first req)) (zerop (first opt)) (not keyp))
        ;; If the whole lambda list is &rest, skip parsing entirely.
        `(let ((,rest ,argstype)) ,@body)
        ;; hard mode
        (let ((gsys (gensym "SYS")) (gargs (gensym "ARGSTYPE"))
              (greq (gensym "REQ")) (glr (gensym "LREQ"))
              (gopt (gensym "OPT"))
              (grest (gensym "REST")) (grb (gensym "REST-BOTTOM-P")))
          `(let* ((,gsys *clasp-system*) (,gargs ,argstype)
                  (,greq (ctype:values-required ,gargs ,gsys))
                  (,glr (length ,greq))
                  (,gopt (ctype:values-optional ,gargs ,gsys))
                  (,grest (ctype:values-rest ,gargs ,gsys))
                  (,grb (ctype:bottom-p ,grest ,gsys)))
             (if (or (and ,grb (< (+ ,glr (length ,gopt)) ,(first req)))
                     ,@(if (or rest keyp)
                           nil
                           `((> ,glr (+ ,(first req) ,(first opt))))))
                 ;; Not enough arguments, or too many
                 ,default
                 ;; Valid call
                 (let* (,@(loop for reqv in (rest req)
                                collect `(,reqv (or (pop ,greq)
                                                    (pop ,gopt) ,grest)))
                        ,@(loop for (optv default -p) on (rest opt) by #'cdddr
                                when -p
                                  collect `(,-p (cond (,greq t)
                                                      ((or ,gopt (not ,grb)) :maybe)
                                                      (t nil)))
                                collect `(,optv
                                          (or (pop ,greq)
                                              (ctype:disjoin ,gsys
                                                             (env:parse-type-specifier
                                                              ',default nil ,gsys)
                                                             (or (pop ,gopt) ,grest)))))
                        ,@(when rest
                            `((,rest (ctype:values ,greq ,gopt ,grest ,gsys))))
                        ,@(loop for (kw var def -p) on (rest keys) by #'cddddr
                                ;; We need a -p for processing
                                for r-p = (or -p (gensym "-P"))
                                ;; KLUDGE: If no default is specified
                                ;; we want T, not NIL
                                for r-def = (or def 't)
                                collect `(,r-p
                                          (kwarg-presence ',kw ,greq ,gopt ,grest
                                                          ,gsys))
                                collect `(,var
                                          (defaulted-kwarg-type
                                           ',kw ,r-p
                                           (env:parse-type-specifier
                                            ',r-def nil ,gsys)
                                           ,greq ,gopt ,grest ,gsys))))
                   ,@body)))))))

(defmacro with-deriver-types (lambda-list argstype &body body)
  `(with-types ,lambda-list ,argstype
     (:default (ctype:values-bottom *clasp-system*))
     ,@body))

;;; Lambda lists are basically ordinary lambda lists, but without &aux
;;; because &aux sucks.
;;; &optional defaults are incorporated into the type bound.
;;; suppliedp parameters will be bound to either T, :MAYBE, or NIL.
;;; T means an argument is definitely provided, :MAYBE that it may or
;;; may not be, and NIL that it definitely isn't.
;;; &rest parameters will be bound to a values type representing all
;;; remaining parameters, which is often more useful than a join.
;;; &key defaults are incorporated into the type bound, so e.g. if
;;; it's derived that a keyword _may_ be provided, within the deriver
;;; the type will be (or provided-type default-type). They are not
;;; evaluated but instead interpreted as type specifiers.
;;; &key is not supported yet.
(defmacro define-deriver (name lambda-list &body body)
  (let* ((fname (make-symbol (format nil "~a-DERIVER" (write-to-string name))))
         (as (gensym "ARGSTYPE")))
    `(progn
       (defun ,fname (,as)
         (block ,(core:function-block-name name)
           (with-deriver-types ,lambda-list ,as ,@body)))
       (setf (gethash ',name *derivers*) ',fname)
       ',name)))

(define-condition inference-error-note (ext:compiler-note)
  ((%fname :initarg :fname :reader inference-error-note-fname)
   (%original-condition :reader inference-error-note-original-condition
                        :initarg :condition))
  (:report
   (lambda (condition stream)
     (format stream "BUG: Serious condition during type inference of ~s:~%~a"
             (inference-error-note-fname condition)
             (inference-error-note-original-condition condition)))))

(defmethod bir-transformations:derive-return-type ((inst bir:abstract-call)
                                                   identity argstype
                                                   (system clasp))
  (let ((deriver (gethash identity *derivers*)))
    (if deriver
        (handler-case
            (funcall deriver argstype)
          (serious-condition (e)
            (cmp:note 'inference-error-note
                      :origin (loop for org = (bir:origin inst)
                                      then (cst:source org)
                                    while (typep org 'cst:cst)
                                    finally (return org))
                      :fname identity :condition e)
            (call-next-method)))
        (call-next-method))))

(defun sv (type) (ctype:single-value type *clasp-system*))

;;; Return the minimum and maximum values of a values type.
;;; NIL maximum means no bound.
(defun values-type-minmax (values-type sys)
  (let* ((nreq (length (ctype:values-required values-type sys))))
    (values nreq
            (if (ctype:bottom-p (ctype:values-rest values-type sys) sys)
                (+ nreq (length (ctype:values-optional values-type sys)))
                nil))))

;;; Derive the type of (typep object 'tspec), where objtype is the derived
;;; type of object.
(defun derive-type-predicate (objtype tspec sys)
  (ctype:single-value
   (let ((type (handler-case (env:parse-type-specifier tspec nil sys)
                 (serious-condition ()
                   (return-from derive-type-predicate
                     (ctype:single-value (ctype:member sys t nil) sys))))))
     (cond ((ctype:subtypep objtype type sys) (ctype:member sys t))
           ((ctype:disjointp objtype type sys) (ctype:member sys nil))
           (t (ctype:member sys t nil))))
   sys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (4) TYPES AND CLASSES

(define-deriver typep (obj type &optional env)
  (declare (ignore env))
  (let ((sys *clasp-system*))
    (if (ctype:member-p sys type)
        (let ((members (ctype:member-members sys type)))
          (if (= (length members) 1)
              (derive-type-predicate obj (first members) sys)
              (ctype:single-value (ctype:member sys t nil) sys)))
        (ctype:single-value (ctype:member sys t nil) sys))))

(define-deriver core::headerp (obj type)
  (let ((sys *clasp-system*))
    (if (ctype:member-p sys type)
        (let ((members (ctype:member-members sys type)))
          (if (= (length members) 1)
              (derive-type-predicate obj (first members) sys)
              (ctype:single-value (ctype:member sys t nil) sys)))
        (ctype:single-value (ctype:member sys t nil) sys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (5) DATA AND CONTROL FLOW

(define-deriver functionp (obj)
  (derive-type-predicate obj 'function *clasp-system*))
(define-deriver compiled-function-p (obj)
  (derive-type-predicate obj 'compiled-function *clasp-system*))

(defun derive-eq/l (arg1 arg2 sys)
  (ctype:single-value
   (if (ctype:disjointp arg1 arg2 sys)
       (ctype:member sys nil)
       ;; our eq/l always return a boolean.
       (ctype:member sys t nil))
   sys))

(define-deriver eq (a1 a2) (derive-eq/l a1 a2 *clasp-system*))
(define-deriver eql (a1 a2) (derive-eq/l a1 a2 *clasp-system*))

(define-deriver identity (arg) (sv arg))

(define-deriver values (&rest args) args)

(defun derive-values-list-aux (ltype sys)
  (cond ((ctype:member-p sys ltype)
         (let ((members (ctype:member-members sys ltype)))
           (cond ((some #'consp members) ; weird, but ok: just give up
                  (ctype:values-top sys))
                 ;; this is a very important case, perhaps surprisingly,
                 ;; because we convert (apply foo bar) to
                 ;; (multiple-value-call foo (values-list bar))
                 ;; and apply to NIL comes up every once in a while,
                 ;; e.g. from &rest parameters.
                 ((member nil members)
                  ;; zero values.
                  (ctype:values nil nil (ctype:bottom sys) sys))
                 (t ; nothing valid
                  (ctype:values-bottom sys)))))
        ((ctype:consp ltype sys)
         (let* ((vr (derive-values-list-aux (ctype:cons-cdr ltype sys) sys))
                (req (ctype:values-required vr sys))
                (opt (ctype:values-optional vr sys))
                (rest (ctype:values-rest vr sys)))
           (ctype:values (list* (ctype:cons-car ltype sys) req) opt rest sys)))
        ((ctype:conjunctionp ltype sys)
         (apply #'ctype:values-conjoin sys
                (mapcar (lambda (sub) (derive-values-list-aux sub sys))
                        (ctype:conjunction-ctypes ltype sys))))
        ((ctype:disjunctionp ltype sys)
         (apply #'ctype:values-disjoin sys
                (mapcar (lambda (sub) (derive-values-list-aux sub sys))
                        (ctype:disjunction-ctypes ltype sys))))
        (t (ctype:values-top sys))))

(define-deriver values-list (list)
  (derive-values-list-aux list *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (8) STRUCTURES

(define-deriver copy-structure (structure) (sv structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (9) CONDITIONS

(define-deriver error (datum &rest arguments)
  (declare (ignore datum arguments))
  (ctype:values-bottom *clasp-system*))

(define-deriver core::etypecase-error (datum types)
  (declare (ignore datum types))
  (ctype:values-bottom *clasp-system*))

;;; I extremely doubt these matter, but why not.
(define-deriver cerror (cfc datum &rest arguments)
  (declare (ignore cfc datum arguments))
  (ctype:single-value (ctype:member *clasp-system* nil) *clasp-system*))
(define-deriver signal (datum &rest arguments)
  (declare (ignore datum arguments))
  (ctype:single-value (ctype:member *clasp-system* nil) *clasp-system*))
(define-deriver warn (datum &rest arguments)
  (declare (ignore datum arguments))
  (ctype:single-value (ctype:member *clasp-system* nil) *clasp-system*))

(define-deriver invoke-debugger (condition)
  (declare (ignore condition))
  (ctype:values-bottom *clasp-system*))

(define-deriver break (&optional format-control &rest args)
  (declare (ignore format-control args))
  (ctype:single-value (ctype:member *clasp-system* nil) *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (10) SYMBOLS

(define-deriver symbolp (object)
  (derive-type-predicate object 'symbol *clasp-system*))
(define-deriver keywordp (object)
  (derive-type-predicate object 'keyword *clasp-system*))

;;; Note that this will apply to the primop as well as to the function.
;;; That's important since it lets the compiler know that it's exactly one value.
(define-deriver symbol-value (symbol)
  (declare (ignore symbol))
  (let ((sys *clasp-system*))
    (ctype:single-value (ctype:top sys) sys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (11) PACKAGES

(define-deriver packagep (object)
  (derive-type-predicate object 'package *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (12) NUMBERS

(define-deriver numberp (object)
  (derive-type-predicate object 'number *clasp-system*))
(define-deriver complexp (object)
  (derive-type-predicate object 'complex *clasp-system*))
(define-deriver realp (object)
  (derive-type-predicate object 'real *clasp-system*))
(define-deriver rationalp (object)
  (derive-type-predicate object 'rational *clasp-system*))
(define-deriver floatp (object)
  (derive-type-predicate object 'float *clasp-system*))
#+short-float
(define-deriver core:short-float-p (object)
  (derive-type-predicate object 'short-float *clasp-system*))
(define-deriver core:single-float-p (object)
  (derive-type-predicate object 'single-float *clasp-system*))
(define-deriver core:double-float-p (object)
  (derive-type-predicate object 'double-float *clasp-system*))
#+long-float
(define-deriver core:long-float-p (object)
  (derive-type-predicate object 'long-float *clasp-system*))
(define-deriver integerp (object)
  (derive-type-predicate object 'integer *clasp-system*))
(define-deriver core:fixnump (object)
  (derive-type-predicate object 'fixnum *clasp-system*))

(define-deriver random-state-p (object)
  (derive-type-predicate object 'random-state *clasp-system*))

(defun contagion (ty1 ty2)
  (ecase ty1
    ((integer)
     (case ty2
       ((integer) ty1)
       ((ratio) 'rational)
       (t ty2)))
    ((ratio)
     (case ty2
       ((integer) 'rational)
       ((ratio) ty1)
       (t ty2)))
    ((rational)
     (case ty2
       ((integer ratio) ty1)
       (t ty2)))
    ((short-float)
     (case ty2
       ((integer ratio rational short-float)
        #+short-float 'short-float
        #-short-float 'single-float)
       (t ty2)))
    ((single-float)
     (case ty2
       ((integer ratio rational short-float) ty1)
       (t ty2)))
    ((double-float)
     (case ty2
       ((integer ratio rational short-float single-float) ty1)
       (t ty2)))
    ((long-float)
     (case ty2
       ((integer ratio rational short-float single-float double-float)
        #+long-float 'long-float
        #-long-float 'double-float)
       (t ty2)))
    ((float)
     (case ty2
       ((integer ratio rational short-float single-float double-float long-float) ty1)
       (t ty2)))
    ((real) ty1)))

;; integer/integer can be a ratio, so this is contagion but lifting to rational.
(defun divcontagion (ty1 ty2)
  (let ((cont (contagion ty1 ty2)))
    (if (member cont '(integer ratio))
        'rational
        cont)))

(defun range+ (ty1 ty2)
  (let* ((sys *clasp-system*)
         (k1 (ctype:range-kind ty1 sys)) (k2 (ctype:range-kind ty2 sys)))
    (multiple-value-bind (low1 lxp1) (ctype:range-low ty1 sys)
      (multiple-value-bind (high1 hxp1) (ctype:range-high ty1 sys)
        (multiple-value-bind (low2 lxp2) (ctype:range-low ty2 sys)
          (multiple-value-bind (high2 hxp2) (ctype:range-high ty2 sys)
            (ctype:range (contagion k1 k2)
                         (if (or (null low1) (null low2))
                             '*
                             (let ((sum (+ low1 low2)))
                               (if (or lxp1 lxp2) (list sum) sum)))
                         (if (or (null high1) (null high2))
                             '*
                             (let ((sum (+ high1 high2)))
                               (if (or hxp1 hxp2) (list sum) sum)))
                         sys)))))))

(defun ty+ (ty1 ty2)
  (if (and (ctype:rangep ty1 *clasp-system*) (ctype:rangep ty2 *clasp-system*))
      (range+ ty1 ty2)
      (env:parse-type-specifier 'number nil *clasp-system*)))

(defun range-negate (ty)
  (let ((sys *clasp-system*))
    (multiple-value-bind (low lxp) (ctype:range-low ty sys)
      (multiple-value-bind (high hxp) (ctype:range-high ty sys)
        (ctype:range (ctype:range-kind ty sys)
                     (cond ((null high) '*)
                           (hxp (list (- high)))
                           (t (- high)))
                     (cond ((null low) '*)
                           (lxp (list (- low)))
                           (t (- low)))
                     sys)))))

(defun ty-negate (ty)
  (if (ctype:rangep ty *clasp-system*)
      (range-negate ty)
      (env:parse-type-specifier 'number nil *clasp-system*)))

;;; Gives the result as the contagion of the inputs, while discarding range
;;; information. This is a very imprecise result, but can be applied fairly
;;; widely. Notably gives up on complexes, returning NUMBER, so this should
;;; be okay to use for e.g. (- complex complex) which can actually be real.
(defun ty-contagion (ty1 ty2)
  (let ((sys *clasp-system*))
    (if (and (ctype:rangep ty1 sys) (ctype:rangep ty2 sys))
        (ctype:range (contagion (ctype:range-kind ty1 sys) (ctype:range-kind ty2 sys))
                     '* '* sys)
        (env:parse-type-specifier 'number nil *clasp-system*))))
(defun ty-divcontagion (ty1 ty2)
  (let ((sys *clasp-system*))
    (if (and (ctype:rangep ty1 sys) (ctype:rangep ty2 sys))
        (ctype:range (divcontagion (ctype:range-kind ty1 sys)
                                   (ctype:range-kind ty2 sys))
                     '* '* sys)
        (env:parse-type-specifier 'number nil *clasp-system*))))

(defun ty-irrat-monotonic1 (ty function &key (inf '*) (sup '*))
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep ty sys)
         (let* ((kind (ctype:range-kind ty sys))
                (mkind
                  (case kind
                    ((integer ratio rational) 'single-float)
                    (t kind))))
           (multiple-value-bind (low lxp) (ctype:range-low ty sys)
             (multiple-value-bind (high hxp) (ctype:range-high ty sys)
               (let ((olow (cond ((not low)
                                  (cond ((not (numberp inf)) inf)
                                        ((eq kind 'double-float) (float inf 0d0))
                                        (t (float inf 0f0))))
                                 (lxp (list (funcall function low)))
                                 (t (funcall function low))))
                     (ohigh (cond ((not high)
                                   (cond ((not (numberp sup)) sup)
                                         ((eq kind 'double-float) (float sup 0d0))
                                         (t (float sup 0f0))))
                                  (hxp (list (funcall function high)))
                                  (t (funcall function high)))))
                 (ctype:range mkind olow ohigh sys)))))
         (ctype:range 'real inf sup sys))
     sys)))

(defun ty-ash (intty countty)
  (let ((sys *clasp-system*))
    (if (and (ctype:rangep intty sys) (ctype:rangep countty sys))
        ;; We could end up with rational/real range inputs, in which case only the
        ;; integers are valid, so we can use ceiling/floor on the bounds.
        (if (and (member (ctype:range-kind intty sys) '(integer rational real))
                 (member (ctype:range-kind countty sys) '(integer rational real)))
            (flet ((pash (integer count default)
                     ;; "protected ASH": Avoid huge numbers when they don't really help.
                     (if (< count (* 2 core:cl-fixnum-bits)) ; arbitrary
                         (ash integer count)
                         default)))
              ;; FIXME: We just ignore exclusive bounds because that's easier and
              ;; doesn't affect the ranges too much. Ideally they should be
              ;; normalized away for integers anyway.
              (let ((ilow  (ctype:range-low  intty   sys))
                    (ihigh (ctype:range-high intty   sys))
                    (clow  (ctype:range-low  countty sys))
                    (chigh (ctype:range-high countty sys)))
                ;; Normalize out non-integers.
                (when ilow  (setf ilow  (ceiling ilow)))
                (when ihigh (setf ihigh (floor   ihigh)))
                (when clow  (setf clow  (ceiling clow)))
                (when chigh (setf chigh (floor   chigh)))
                ;; ASH with a positive count increases magnitude while a negative
                ;; count decreases it. Therefore: If the integer can be negative,
                ;; the low point of the range must be (ASH ILOW CHIGH). Even if
                ;; CHIGH is negative, this can at worst result in 0, which is <=
                ;; any lower bound from IHIGH. If the integer can't be negative,
                ;; low bound must be (ASH ILOW CLOW). Vice versa for the upper bound.
                (ctype:range 'integer
                             (cond ((not ilow) '*)
                                   ((< ilow 0)  (if chigh (pash ilow chigh  '*) '*))
                                   ((> ilow 0)  (if clow  (pash ilow clow    0)  0))
                                   (t 0))
                             (cond ((not ihigh) '*)
                                   ((< ihigh 0) (if clow  (pash ihigh clow  -1) -1))
                                   ((> ihigh 0) (if chigh (pash ihigh chigh '*) '*))
                                   (t 0))
                             sys)))
            (ctype:bottom sys))
        (ctype:range 'integer '* '* sys))))

;;;

;;; Split into two intervals, one wholly less than zero and one greater.
;;; If either range is empty, NIL is returned for it instead.
(defun range->intervals-for-reciprocal (range sys)
  (multiple-value-bind (low lxp) (ctype:range-low range sys)
    (multiple-value-bind (high hxp) (ctype:range-high range sys)
      (if (eq (ctype:range-kind range sys) 'integer)
          ;; Integer ranges we treat specially when they include 0,
          ;; because the interval arithmetic doesn't understand discreteness.
          ;; For example, the reciprocal of an (integer -7 7) is a rational
          ;; between -1 and 1, because (/ 1) = 1; but the reciprocal of a
          ;; (rational -7 7) is unbounded as it approaches zero.
          ;; We also normalize exclusive bounds while we're at it.
          (values
           (if (and low (>= low (if lxp -1 0)))
               nil
               (make-interval (cond ((not low) low)
                                    (lxp (1+ low))
                                    (t low))
                              (cond ((or (not high) (>= high 0)) -1)
                                    (hxp (1- high))
                                    (t high))))
           (if (and high (<= high (if hxp 1 0)))
               nil
               (make-interval (cond ((or (not low) (<= low 0)) 1)
                                    (lxp (1+ low))
                                    (t low))
                              (cond ((not high) high)
                                    (hxp (1- high))
                                    (t high)))))
          (values
           (if (and low (>= low 0))
               nil
               (make-interval (cond ((not low) low)
                                    (lxp (list low))
                                    (t low))
                              (cond ((or (not high) (>= high 0)) '(0))
                                    (hxp (list high))
                                    (t high))))
           (if (and high (<= high 0))
               nil
               (make-interval (cond ((or (not low) (<= low 0)) '(0))
                                    (lxp (list low))
                                    (t low))
                              (cond ((not high) high)
                                    (hxp (list high))
                                    (t high)))))))))

;;; Given two range types, return an interval for the result.
;;; We take types rather than intervals because the divisor being an integer
;;; can restrict the result when it crosses zero.
(defun range-divide (n1 n2 sys)
  (let* ((int1 (range->interval n1 sys)))
    (multiple-value-bind (below2 above2)
        (range->intervals-for-reciprocal n2 sys)
      (let ((rbelow (if below2
                        (interval-negate
                         (interval*-1-pos
                          (interval-reciprocal-+ (interval-negate below2))
                          int1))
                        nil))
            (rabove (if above2
                        (interval*-1-pos (interval-reciprocal-+ above2) int1)
                        nil)))
        (cond ((and rbelow rabove) (interval-merge rbelow rabove))
              (rbelow rbelow)
              (rabove rabove)
              ;; arises from division by zero.
              ;; this can result in infinities and NaN, so we punt a bit.
              ;; FIXME: We could be a bit more intelligent, e.g. NIL type
              ;; for rationals, and get the sign of infinities.
              (t (make-interval nil nil)))))))

(defun floor-remainder (dividend divisor)
  ;; Technically the range of the remainder can change based on the range of the
  ;; dividend, like when the dividend range is smaller than a known divisor,
  ;; but that probably doesn't come up enough to be interesting.
  ;; For FLOOR, the remainder always has the same sign as the divisor.
  ;; Note that we don't check for division by zero here. That's mostly out of
  ;; laziness, but it should be okay since the quotient type does more checking.
  (declare (ignore dividend))
  (make-interval (let ((low (bound-parts (interval-low divisor))))
                   (cond ((not low) low)
                         ((>= low 0) 0)
                         ;; these are always exclusive.
                         ;; e.g (floor x -2) never has a remainder of -2.
                         (t (list low))))
                 (let ((high (bound-parts (interval-high divisor))))
                   (cond ((not high) high)
                         ((<= high 0) 0)
                         (t (list high))))))

(defun ceiling-remainder (dividend divisor)
  (declare (ignore dividend))
  ;; The remainder has the opposite sign of the divisor.
  (make-interval (let ((high (bound-parts (interval-high divisor))))
                   (cond ((not high) high)
                         ((<= high 0) 0)
                         (t (list (- high)))))
                 (let ((low (bound-parts (interval-low divisor))))
                   (cond ((not low) low)
                         ((>= low 0) 0)
                         (t (list (- low)))))))

(defun truncate-remainder (dividend divisor)
  ;; The remainder has the same sign as the dividend. A bit trickier.
  ;; First, get the divisor bound with the largest magnitude.
  (let* ((low (bound-parts (interval-low divisor)))
         (high (bound-parts (interval-high divisor)))
         (max (if (or (not low) (not high))
                  nil
                  (max (abs low) (abs high)))))
    (let ((low (interval-low dividend)) (high (interval-high dividend)))
      (cond ((and low (>= low 0))
             ;; Dividend is positive, so the remainder must be.
             (make-interval 0 (if max (list max) nil)))
            ((and high (<= high 0))
             ;; Dividend is negative, so the remainder must be.
             (make-interval (if max (list (- max)) nil) 0))
            ;; Dividend is either sign, so we don't know.
            (max (make-interval (list (- max)) (list max)))
            (t (make-interval nil nil))))))

(defun derive-floor-etc (dividend divisor quokindfun quofun remfun sys)
  (if (and (ctype:rangep dividend sys) (ctype:rangep divisor sys))
      ;; The CLHS actually only says that the remainder
      ;; is a float if an argument is a float, i.e. it doesn't
      ;; specify that it has to be a double given doubles, etc.
      ;; But in Clasp we choose to use the usual contagion rules.
      (let* ((dividend-kind (ctype:range-kind dividend sys))
             (divisor-kind (ctype:range-kind divisor sys))
             (rkind (contagion dividend-kind divisor-kind)))
        (ctype:values
         (list (interval->range
                (funcall quofun (range-divide dividend divisor sys))
                (funcall quokindfun dividend-kind divisor-kind) sys)
               (interval->range
                (funcall remfun (range->interval dividend sys)
                         (range->interval divisor sys))
                rkind sys))
         nil (ctype:bottom sys) sys))
      (ctype:values (list (ctype:range (funcall quokindfun 'real 'real)
                                       '* '* sys)
                          (env:parse-type-specifier 'real nil sys))
                    nil (ctype:bottom sys) sys)))

(defun floor-quokind (k1 k2) (declare (ignore k1 k2)) 'integer)

(define-deriver truncate (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'floor-quokind #'interval-truncate #'truncate-remainder
                    *clasp-system*))
(define-deriver floor (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'floor-quokind #'interval-floor #'floor-remainder
                    *clasp-system*))
(define-deriver ceiling (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'floor-quokind #'interval-ceiling #'ceiling-remainder
                    *clasp-system*))

(define-deriver mod (number divisor)
  (let ((sys *clasp-system*))
    (if (and (ctype:rangep number sys) (ctype:rangep divisor sys))
        (ctype:single-value
         (interval->range (floor-remainder (range->interval number sys)
                                           (range->interval divisor sys))
                          (contagion (ctype:range-kind number sys)
                                     (ctype:range-kind divisor sys))
                          sys)
         sys)
        (ctype:single-value (env:parse-type-specifier 'real nil sys) sys))))
(define-deriver rem (number divisor)
  (let ((sys *clasp-system*))
    (if (and (ctype:rangep number sys) (ctype:rangep divisor sys))
        (ctype:single-value
         (interval->range (truncate-remainder (range->interval number sys)
                                           (range->interval divisor sys))
                          (contagion (ctype:range-kind number sys)
                                     (ctype:range-kind divisor sys))
                          sys)
         sys)
        (ctype:single-value (env:parse-type-specifier 'real nil sys) sys))))

;;; The specification of the quotient's type in the CLHS is really weird, but
;;; Clasp does the following: If both arguments are rational, a single float.
;;; Otherwise, a float of the largest format among the arguments.
(defun ffloor-quokind (k1 k2)
  (cond ((or (member k1 '(float real)) (member k2 '(float real))) 'float)
        ((and (member k1 '(integer ratio real)) (member k2 '(integer ratio real)))
         'single-float)
        (t (contagion k1 k2))))

(define-deriver ffloor (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'ffloor-quokind #'interval-floor #'floor-remainder
                    *clasp-system*))
(define-deriver fceiling (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'ffloor-quokind #'interval-ceiling #'ceiling-remainder
                    *clasp-system*))
(define-deriver ftruncate (dividend &optional (divisor (integer 1 1)))
  (derive-floor-etc dividend divisor
                    #'ffloor-quokind #'interval-truncate #'truncate-remainder
                    *clasp-system*))

(define-deriver core:two-arg-+ (n1 n2) (sv (ty+ n1 n2)))
(define-deriver core:negate (arg) (sv (ty-negate arg)))
(define-deriver core:two-arg-- (n1 n2) (sv (ty+ n1 (ty-negate n2))))

(defun range->interval (range sys)
  (multiple-value-bind (low lxp) (ctype:range-low range sys)
    (multiple-value-bind (high hxp) (ctype:range-high range sys)
      (make-interval (if lxp (list low) low) (if hxp (list high) high)))))

(defun coerce-bound (bound kind)
  (flet ((%coerce (num)
           (ecase kind
             ((integer rational) (rational num))
             ((short-float single-float double-float long-float float) (coerce num kind))
             ((real) num))))
    (cond ((null bound) '*)
          ((consp bound) (list (%coerce (car bound))))
          (t (%coerce bound)))))

(defun interval->range (interval kind sys)
  (ctype:range kind
               (coerce-bound (interval-low interval) kind)
               (coerce-bound (interval-high interval) kind) sys))

(define-deriver core:two-arg-* (n1 n2)
  (let* ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep n1 sys) (ctype:rangep n2 sys))
         (let ((i1 (range->interval n1 sys)) (i2 (range->interval n2 sys)))
           (interval->range (interval* i1 i2)
                            (contagion (ctype:range-kind n1 sys)
                                       (ctype:range-kind n2 sys))
                            sys))
         (env:parse-type-specifier 'number nil sys))
     sys)))

(define-deriver core:two-arg-/ (n1 n2)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep n1 sys) (ctype:rangep n2 sys))
         (interval->range (range-divide n1 n2 sys)
                          (divcontagion (ctype:range-kind n1 sys)
                                        (ctype:range-kind n2 sys))
                          sys)
         (env:parse-type-specifier 'number nil sys))
     sys)))
(define-deriver core:reciprocal (n)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep n sys)
         (let* ((kind (ctype:range-kind n sys))
                (rkind (if (eq kind 'integer) 'rational kind)))
           (multiple-value-bind (below above)
               (range->intervals-for-reciprocal n sys)
             (let ((rbelow
                     (if below
                         (interval->range
                          (interval-negate
                           (interval-reciprocal-+ (interval-negate below)))
                          rkind sys)
                         nil))
                   (rabove
                     (if above
                         (interval-reciprocal-+ above)
                         nil)))
               (cond (rabove 
                     (interval->range
                       (if rbelow
                           (interval-merge rbelow rabove)
                           rabove)
                       rkind sys))
                     (rbelow (interval->range rbelow rkind sys))
                     ;; Both being NIL happens if the input range is all zero.
                     ;; As in / above, this can result in infinities rather than
                     ;; being an error, so we punt.
                     (t (ctype:range rkind '* '* sys))))))
         (env:parse-type-specifier 'number nil sys))
     sys)))

(define-deriver exp (arg) (ty-irrat-monotonic1 arg #'exp :inf 0f0))

(define-deriver expt (x y)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep x sys) (ctype:rangep y sys))
         (ctype:range
          (ecase (ctype:range-kind x sys)
            ((single-float) (if (eq (ctype:range-kind y sys) 'double-float)
                                'double-float 'single-float))
            ((double-float) 'double-float)
            ((long-float) 'long-float)
            ((float) 'float)
            ((integer)
             (let ((ykind (ctype:range-kind y sys)))
               (case ykind
                 ((integer rational) 'rational) ; e.g. (expt 2 -3)
                 (otherwise ykind))))
            ((rational)
             (let ((ykind (ctype:range-kind y sys)))
               (case ykind
                 ((integer rational) 'rational)
                 (otherwise ykind))))
            ((real) 'real))
          '* '* sys)
         (env:parse-type-specifier 'number nil sys))
     sys)))

(defun ty-boundbelow-irrat-monotonic1 (ty function lowbound &key (inf '*) (sup '*))
  (let ((sys *clasp-system*))
    (if (ctype:rangep ty sys)
        (let ((low (ctype:range-low ty sys)))
          (if (and low (>= low lowbound))
              (ty-irrat-monotonic1 ty function :inf inf :sup sup)
              (ctype:single-value
               (env:parse-type-specifier 'number nil sys) sys)))
        (ctype:single-value (env:parse-type-specifier 'number nil sys) sys))))

(defun ty-bound-irrat-monotonic1 (ty function lowb highb &key (inf '*) (sup '*))
  (let ((sys *clasp-system*))
    (if (ctype:rangep ty sys)
        (let ((low (ctype:range-low ty sys))
              (high (ctype:range-high ty sys)))
          (if (and low (>= low lowb)
                   high (<= high highb))
              (ty-irrat-monotonic1 ty function :inf inf :sup sup)
              (ctype:single-value
               (env:parse-type-specifier 'number nil sys) sys)))
        (ctype:single-value (env:parse-type-specifier 'number nil sys) sys))))

(define-deriver sqrt (arg)
  (ty-boundbelow-irrat-monotonic1 arg #'sqrt 0 :inf 0f0))

(define-deriver log (arg &optional (base nil basep))
  (declare (ignore base))
  (if basep
      ;; FIXME
      (let ((sys *clasp-system*))
        (ctype:single-value (env:parse-type-specifier 'number nil sys) sys))
      (ty-boundbelow-irrat-monotonic1 arg #'log 0)))

;;; If the argument is a real, return [-1,1]. otherwise just NUMBER
;;; Technically the range could be reduced sometimes, but figuring out the
;;; exact values is kind of a pain and it doesn't seem that useful.
(defun derive-sincos (arg)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep arg sys)
         (let ((kind (ctype:range-kind arg sys)))
           (when (member kind '(integer rational))
             (setf kind 'single-float))
           (ctype:range kind (coerce -1 kind) (coerce 1 kind) sys))
         (env:parse-type-specifier 'number nil sys))
     sys)))
(define-deriver sin (arg) (derive-sincos arg))
(define-deriver cos (arg) (derive-sincos arg))

(defun ty-trig (arg)
  ;; Return an unbounded real range if given a real, else just NUMBER.
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep arg sys)
         (let ((kind (ctype:range-kind arg sys)))
           (when (member kind '(integer rational))
             (setf kind 'single-float))
           (ctype:range kind '* '* sys))
         (env:parse-type-specifier 'number nil sys))
     sys)))

(define-deriver tan (arg) (ty-trig arg))

(define-deriver asin (arg)
  (ty-bound-irrat-monotonic1 arg #'asin -1 1 :inf (- (/ pi 2)) :sup (/ pi 2)))
(define-deriver acos (arg)
  ;; TODO: we could get better types here, since acos is monotone decreasing.
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep arg sys)
         (let ((kind (ctype:range-kind arg sys))
               (low (ctype:range-low arg sys))
               (high (ctype:range-high arg sys)))
           (if (and low (>= low -1d0) high (<= high 1d0))
               (ecase kind
                 ((integer rational single-float)
                  (ctype:range 'single-float 0f0 (float pi 0f0) sys))
                 ((double-float) (ctype:range 'double-float 0d0 pi sys))
                 ((long-float) (ctype:range 'long-float 0l0 pi sys))
                 ((float real) (ctype:range 'float 0d0 pi sys)))
               (env:parse-type-specifier 'number nil sys)))
         (env:parse-type-specifier 'number nil sys))
     sys)))

(define-deriver sinh (arg) (ty-irrat-monotonic1 arg #'sinh))
(define-deriver cosh (arg) (ty-trig arg)) ; FIXME: limit to (1, \infty]

(define-deriver tanh (arg) (ty-irrat-monotonic1 arg #'tanh :inf -1f0 :sup 1f0))

(define-deriver asinh (arg) (ty-irrat-monotonic1 arg #'asinh))
(define-deriver acosh (arg) (ty-boundbelow-irrat-monotonic1 arg #'acosh 1 :inf 0f0))
(define-deriver atanh (arg) (ty-bound-irrat-monotonic1 arg #'atanh -1 1))

(define-deriver abs (arg)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep arg sys)
         (let ((kind (ctype:range-kind arg sys)))
           (multiple-value-bind (low lxp) (ctype:range-low arg sys)
             (multiple-value-bind (high hxp) (ctype:range-high arg sys)
               (ctype:range kind
                            (cond ((or (not low) (and low (minusp low)))
                                   (case kind
                                     ((single-float) 0f0)
                                     ((double-float float) 0d0)
                                     ((long-float) 0l0)
                                     (t 0)))
                                  ((or (not high) (< low (abs high)))
                                   (if lxp (list low) low))
                                  (t (if hxp (list (abs high)) (abs high))))
                            (cond ((or (not high) (not low)) '*)
                                  ((< (abs low) (abs high))
                                   (if hxp (list (abs high)) (abs high)))
                                  (t (if lxp (list (abs low)) (abs low))))
                            sys))))
         (env:parse-type-specifier 'number nil sys))
     sys)))

(define-deriver ash (num shift) (sv (ty-ash num shift)))
(define-deriver core:ash-left (num shift) (sv (ty-ash num shift)))
(define-deriver core:ash-right (num shift) (sv (ty-ash num (ty-negate shift))))

(defun derive-to-float (realtype format sys)
  ;; TODO: disjunctions
  (if (ctype:rangep realtype sys)
      (multiple-value-bind (low lxp) (ctype:range-low realtype sys)
        (multiple-value-bind (high hxp) (ctype:range-high realtype sys)
          (ctype:range format
                       (cond ((not low) '*)
                             (lxp (list (coerce low format)))
                             (t (coerce low format)))
                       (cond ((not high) '*)
                             (hxp (list (coerce high format)))
                             (t (coerce high format)))
                       sys)))
      (ctype:range format '* '* sys)))

(define-deriver float (num &optional (proto nil protop))
  (let* ((sys *clasp-system*)
         (floatt (ctype:range 'float '* '* sys)))
    (flet ((float1 ()
             ;; TODO: disjunctions
             (cond ((ctype:subtypep num floatt sys) num) ; no coercion
                   ((ctype:subtypep num (ctype:negate floatt sys) sys)
                    (derive-to-float num 'single-float sys))
                   (t floatt)))
           (float2 ()
             (cond ((ctype:subtypep proto (ctype:range 'single-float '* '* sys) sys)
                    (derive-to-float num 'single-float sys))
                   ((ctype:subtypep proto (ctype:range 'double-float '* '* sys) sys)
                    (derive-to-float num 'double-float sys))
                   ((ctype:subtypep proto (ctype:range 'long-float '* '* sys) sys)
                    (derive-to-float num 'long-float sys))
                   (t floatt))))
      (ctype:single-value
       (cond ((eq protop t) (float2)) ; definitely supplied
             ((eq protop :maybe)
              (ctype:disjoin sys (float1) (float2)))
             (t (float1)))
       sys))))
(define-deriver core:to-single-float (num)
  (let ((sys *clasp-system*))
    (ctype:single-value (derive-to-float num 'single-float sys) sys)))
(define-deriver core:to-double-float (num)
  (let ((sys *clasp-system*))
    (ctype:single-value (derive-to-float num 'double-float sys) sys)))
#+long-float
(define-deriver core:to-long-float (num)
  (let ((sys *clasp-system*))
    (ctype:single-value (derive-to-float num 'long-float sys) sys)))

(define-deriver random (max &optional random-state)
  (declare (ignore random-state))
  (let ((sys *clasp-system*))
    (ctype:single-value
     (cond ((ctype:rangep max sys)
            (let* ((kind (ctype:range-kind max sys))
                   (phigh (ctype:range-high max sys)) ; x-p irrelevant here
                   (high (if phigh (list phigh) '*)))
              (case kind
                ((integer ratio rational) (ctype:range kind 0 high sys))
                ((real) (ctype:range kind 0 high sys))
                (t (ctype:range kind (coerce 0 kind) high sys)))))
           ((subtypep max (ctype:range 'integer 0 most-positive-fixnum sys))
            (ctype:range 'integer 0 most-positive-fixnum sys))
           ((subtypep max (ctype:range 'single-float 0f0 '* sys))
            (ctype:range 'single-float 0f0 '* sys))
           ((subtypep max (ctype:range 'double-float 0d0 '* sys))
            (ctype:range 'double-float 0d0 '* sys))
           ((subtypep max (ctype:range 'long-float 0d0 '* sys))
            (ctype:range 'long-float 0l0 '* sys))
           (t (env:parse-type-specifier '(real 0) nil sys)))
     sys)))

;;; Get inclusive integer bounds from a type. NIL for unbounded.
;;; FIXME: For integer types we should just normalize away exclusivity at parse
;;; time, really.
(defun normalize-integer-bounds (ranget sys)
  (let ((kind (ctype:range-kind ranget sys)))
    (multiple-value-bind (low lxp) (ctype:range-low ranget sys)
      (multiple-value-bind (high hxp) (ctype:range-high ranget sys)
        (ecase kind
          ((integer) (values (if (and low lxp) (1+ low) low) (if (and high hxp) (1- high) high)))
          ((rational real)
           (values (if low
                       (multiple-value-bind (clow crem) (ceiling low)
                         (if (and (zerop crem) lxp) (1+ clow) clow))
                       low)
                   (if high
                       (multiple-value-bind (fhigh frem) (floor high)
                         (if (and (zerop frem) hxp) (1- fhigh) fhigh))
                       high))))))))

(define-deriver logcount (arg)
  ;; not optimal, but should be fine.
  ;; example non optimality: (logcount (integer 10 15)) could be (integer 2 4)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep arg sys)
              (member (ctype:range-kind arg sys) '(integer rational real)))
         (multiple-value-bind (low high) (normalize-integer-bounds arg sys)
           (if (and low high)
               (ctype:range 'integer
                            (if (or (> low 0) (< high -1)) 1 0)
                            (max (integer-length low) (integer-length high))
                            sys)
               (ctype:range 'integer '* '* sys)))
         (ctype:range 'integer '* '* sys))
     sys)))

(define-deriver integer-length (arg)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep arg sys)
              (member (ctype:range-kind arg sys) '(integer rational real)))
         (multiple-value-bind (low high) (normalize-integer-bounds arg sys)
           (multiple-value-bind (nlow nhigh)
               ;; We compute bounds based on integer-length being nondecreasing
               ;; from 0 on up and from -1 on down. So if we're entirely positive
               ;; or negative we just work monotonically, otherwise min is zero
               ;; and max is whatever's biggest.
               (cond ((and low (> low 0))
                      ;; entirely positive range.
                      (values (integer-length low)
                              (if high (integer-length high) '*)))
                     ((and high (< high 0))
                      ;; entirely negative
                      (values (integer-length high)
                              (if low (integer-length low) '*)))
                     (t
                      ;; zero-crossing
                      (values 0 (if (and low high)
                                    (max (integer-length low) (integer-length high))
                                    '*))))
             (ctype:range 'integer nlow nhigh sys)))
         (ctype:range 'integer 0 '* sys))
     sys)))

;;; LOGNOT (in Lisp's unbounded conception) is monotonic decreasing.
(define-deriver lognot (arg)
  (let* ((sys *clasp-system*))
    (ctype:single-value
     (if (and (ctype:rangep arg sys)
              (member (ctype:range-kind arg sys) '(integer rational real)))
         (multiple-value-bind (low high) (normalize-integer-bounds arg sys)
           (ctype:range 'integer
                        (if high (lognot high) '*)
                        (if low (lognot low) '*)
                        sys))
         (ctype:range 'integer '* '* sys))
     sys)))

;;; Getting good bounds for these functions is kind of nontrivial.
;;; For now we just mark them as returning fixnums if given them.
;;; TODO. Check Hacker's Delight and SBCL's compiler/bitops-derive-type.lisp.

(defmacro define-log2-deriver (name)
  `(define-deriver ,name (int1 int2)
     (let* ((sys *clasp-system*)
            (fixnum (ctype:range 'integer
                                 most-negative-fixnum most-positive-fixnum sys)))
       (ctype:single-value (if (and (ctype:subtypep int1 fixnum sys)
                                    (ctype:subtypep int2 fixnum sys))
                               fixnum
                               (ctype:range 'integer '* '* sys))
                           sys))))
(define-log2-deriver core:logand-2op)
(define-log2-deriver core:logior-2op)
(define-log2-deriver core:logxor-2op)
(define-log2-deriver logandc1)
(define-log2-deriver logandc2)
(define-log2-deriver logorc1)
(define-log2-deriver logorc2)
(define-log2-deriver core:logeqv-2op)
(define-log2-deriver lognand)
(define-log2-deriver lognor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (13) CHARACTERS

(define-deriver characterp (object)
  (derive-type-predicate object 'character *clasp-system*))

(define-deriver standard-char-p (object)
  ;; We could be more specific here, since standard-char-p of a non-character
  ;; is actually an error, but this valid.
  (derive-type-predicate object 'standard-char-p *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (14) CONSES

(define-deriver cons (car cdr)
  (declare (ignore car cdr))
  ;; We can't forward the argument types into the cons type, since we don't
  ;; know if this cons will be mutated. So we just return the CONS type.
  ;; This is useful so that the compiler understands that CONS definitely
  ;; returns a CONS and it does not need to insert any runtime checks.
  (let* ((sys clasp-cleavir:*clasp-system*) (top (ctype:top sys)))
    (ctype:single-value (ctype:cons top top sys) sys)))

(define-deriver consp (obj)
  (derive-type-predicate obj 'cons *clasp-system*))
(define-deriver atom (obj)
  (derive-type-predicate obj 'atom *clasp-system*))

(define-deriver listp (obj)
  (derive-type-predicate obj 'list *clasp-system*))

(defun type-car (type sys)
  (if (ctype:consp type sys)
      (ctype:cons-car type sys)
      (ctype:top sys)))
(defun type-cdr (type sys)
  (if (ctype:consp type sys)
      (ctype:cons-cdr type sys)
      (ctype:top sys)))
(defmacro defcr-type (name &rest ops)
  `(define-deriver ,name (obj)
     (let ((sys clasp-cleavir:*clasp-system*))
       (ctype:single-value
        ,(labels ((rec (ops)
                    (if ops
                        `(,(first ops) ,(rec (rest ops)) sys)
                        'obj)))
           (rec ops))
        sys))))
(defcr-type car type-car)
(defcr-type cdr type-cdr)
(defcr-type caar type-car type-car)
(defcr-type cadr type-car type-cdr)
(defcr-type cdar type-cdr type-car)
(defcr-type cddr type-cdr type-cdr)
(defcr-type caaar type-car type-car type-car)
(defcr-type caadr type-car type-car type-cdr)
(defcr-type cadar type-car type-cdr type-car)
(defcr-type caddr type-car type-cdr type-cdr)
(defcr-type cdaar type-cdr type-car type-car)
(defcr-type cdadr type-cdr type-car type-cdr)
(defcr-type cddar type-cdr type-cdr type-car)
(defcr-type cdddr type-cdr type-cdr type-cdr)
(defcr-type caaaar type-car type-car type-car type-car)
(defcr-type caaadr type-car type-car type-car type-cdr)
(defcr-type caadar type-car type-car type-cdr type-car)
(defcr-type caaddr type-car type-car type-cdr type-cdr)
(defcr-type cadaar type-car type-cdr type-car type-car)
(defcr-type cadadr type-car type-cdr type-car type-cdr)
(defcr-type caddar type-car type-cdr type-cdr type-car)
(defcr-type cadddr type-car type-cdr type-cdr type-cdr)
(defcr-type cdaaar type-cdr type-car type-car type-car)
(defcr-type cdaadr type-cdr type-car type-car type-cdr)
(defcr-type cdadar type-cdr type-car type-cdr type-car)
(defcr-type cdaddr type-cdr type-car type-cdr type-cdr)
(defcr-type cddaar type-cdr type-cdr type-car type-car)
(defcr-type cddadr type-cdr type-cdr type-car type-cdr)
(defcr-type cdddar type-cdr type-cdr type-cdr type-car)
(defcr-type cddddr type-cdr type-cdr type-cdr type-cdr)

(defcr-type rest type-cdr)
(defcr-type first type-car)
(defcr-type second type-car type-cdr)
(defcr-type third type-car type-cdr type-cdr)
(defcr-type fourth type-car type-cdr type-cdr type-cdr)
(defcr-type fifth type-car type-cdr type-cdr type-cdr type-cdr)
(defcr-type sixth type-car type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type seventh type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type eighth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type ninth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type tenth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr
  type-cdr)

(define-deriver list (&rest args)
  (let* ((sys *clasp-system*) (top (ctype:top sys)))
    (ctype:single-value
     (multiple-value-bind (min max) (values-type-minmax args sys)
       (cond ((> min 0) (ctype:cons top top sys))
             ((and max (zerop max)) (ctype:member sys nil))
             (t (ctype:disjoin sys (ctype:member sys nil)
                               (ctype:cons top top sys)))))
     sys)))

(define-deriver list* (arg &rest args)
  (let* ((sys *clasp-system*) (top (ctype:top sys)))
    (ctype:single-value
     (multiple-value-bind (min max) (values-type-minmax args sys)
       (cond ((> min 1) (ctype:cons top top sys))
             ((and max (zerop max)) arg)
             (t
              (ctype:disjoin sys arg (ctype:cons top top sys)))))
     sys)))

(define-deriver endp (obj) (derive-type-predicate obj 'null *clasp-system*))
(define-deriver null (obj) (derive-type-predicate obj 'null *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (15) ARRAYS

(define-deriver make-array (dimensions
                            &key (element-type (eql t))
                            initial-element initial-contents
                            (adjustable (eql nil))
                            (fill-pointer (eql nil)) (displaced-to (eql nil))
                            displaced-index-offset)
  (declare (ignore displaced-index-offset initial-element initial-contents))
  (let* ((sys *clasp-system*)
         (etypes (if (ctype:member-p sys element-type)
                     (ctype:member-members sys element-type)
                     '*))
         (complexity
           (let ((null (ctype:member sys nil)))
             (if (and (ctype:subtypep adjustable null sys)
                      (ctype:subtypep fill-pointer null sys)
                      (ctype:subtypep displaced-to null sys))
                 'simple-array
                 'array)))
         (dimensions
           (cond (;; If the array is adjustable, dimensions could change.
                  (eq complexity 'array) '*)
                 (;; FIXME: Clasp's subtypep returns NIL NIL on
                  ;; (member 23), (or fixnum (cons fixnum null)). Ouch!
                  (ctype:subtypep dimensions
                                  (env:parse-type-specifier 'fixnum nil sys)
                                  sys)
                  ;; TODO: Check for constant?
                  '(*))
                 ;; FIXME: Could be way better.
                 (t '*))))
    (ctype:single-value
     (cond ((eq etypes '*)
            (ctype:array etypes dimensions complexity sys))
           ((= (length etypes) 1)
            (ctype:array (first etypes) dimensions complexity sys))
           (t
            (apply #'ctype:disjoin sys
                   (loop for et in etypes
                         collect (ctype:array et dimensions complexity sys)))))
     sys)))

;;; This is partly around for a KLUDGEy reason - the transform in bir-to-bmir
;;; won't fire unless the compiler tracks check-bound's identity, and it will
;;; not do that just because it has a bir-to-bmir lowering. FIXME.
(define-deriver core:check-bound (vector bound index)
  (declare (ignore vector bound index))
  ;; We can improve this a lot based on the types of index and bound, and knowing
  ;; that bound will always be constant in practice. Might not matter though.
  ;; (We should, however, put a higher level transform on this to eliminate it
  ;;  when the type bounds work out to guarantee the bound.)
  (let ((sys *clasp-system*))
    (ctype:single-value (ctype:range 'integer 0 (1- array-dimension-limit) sys) sys)))

(defun type-aet (type sys)
  (if (ctype:arrayp type sys)
      (ctype:array-element-type type sys)
      (ctype:top sys)))

(defun derive-aref (array indices)
  (declare (ignore indices))
  (let ((sys *clasp-system*))
    (ctype:single-value (type-aet array sys) sys)))

(define-deriver aref (array &rest indices) (derive-aref array indices))
(define-deriver (setf aref) (value array &rest indices)
  (declare (ignore array indices))
  (sv value))

(define-deriver core:vref (vector index) (derive-aref vector (list index)))
(define-deriver (setf core:vref) (value vector index)
  (declare (ignore vector index))
  (sv value))

(defun type-array-rank-if-constant (type sys)
  (if (ctype:arrayp type sys)
      (let ((dims (ctype:array-dimensions type sys)))
        (if (eq dims '*)
            nil
            (length dims)))
      nil))

(define-deriver array-rank (array)
  (let ((sys *clasp-system*))
    (ctype:single-value (let ((rank (type-array-rank-if-constant array sys)))
                          (if rank
                              (ctype:range 'integer rank rank sys)
                              (ctype:range 'integer 0 (1- array-rank-limit) sys)))
                        sys)))

(define-deriver arrayp (object)
  (derive-type-predicate object 'array *clasp-system*))

(define-deriver row-major-aref (array index)
  (declare (ignore index))
  (sv (type-aet array *clasp-system*)))
(define-deriver (setf row-major-aref) (value array index)
  (declare (ignore array index))
  (sv value))

(define-deriver vectorp (object)
  (derive-type-predicate object 'vector *clasp-system*))

(define-deriver bit-vector-p (obj)
  (derive-type-predicate obj 'bit-vector *clasp-system*))
(define-deriver simple-bit-vector-p (obj)
  (derive-type-predicate obj 'simple-bit-vector *clasp-system*))

(define-deriver core:data-vector-p (obj)
  (derive-type-predicate obj 'core:abstract-simple-vector *clasp-system*))

(macrolet ((def (fname etype)
             `(define-deriver ,fname (&rest ignore)
                (declare (ignore ignore))
                (let ((sys *clasp-system*))
                  (ctype:single-value
                   (ctype:array ',etype '(*) 'simple-array sys)
                   sys)))))
  (def core:make-simple-vector-t t)
  (def core:make-simple-vector-bit bit)
  (def core:make-simple-vector-base-char base-char)
  (def core:make-simple-vector-character character)
  (def core:make-simple-vector-single-float single-float)
  (def core:make-simple-vector-double-float double-float)
  #+short-float
  (def core:make-simple-vector-short-float short-float)
  #+long-float
  (def core:make-simple-vector-long-float long-float)
  (def core:make-simple-vector-int2 ext:integer2)
  (def core:make-simple-vector-byte2 ext:byte2)
  (def core:make-simple-vector-int4 ext:integer4)
  (def core:make-simple-vector-byte4 ext:byte4)
  (def core:make-simple-vector-int8 ext:integer8)
  (def core:make-simple-vector-byte8 ext:byte8)
  (def core:make-simple-vector-int16 ext:integer16)
  (def core:make-simple-vector-byte16 ext:byte16)
  (def core:make-simple-vector-int32 ext:integer32)
  (def core:make-simple-vector-byte32 ext:byte32)
  (def core:make-simple-vector-int64 ext:integer64)
  (def core:make-simple-vector-byte64 ext:byte64)
  (def core:make-simple-vector-fixnum fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (16) STRINGS

(define-deriver simple-string-p (obj)
  (derive-type-predicate obj 'simple-string *clasp-system*))

(define-deriver stringp (obj)
  (derive-type-predicate obj 'string *clasp-system*))

(define-deriver make-string (size &key (initial-element character)
                                  (element-type (eql character)))
  (declare (ignore initial-element))
  (let* ((sys *clasp-system*)
         (etypes (if (ctype:member-p sys element-type)
                     (ctype:member-members sys element-type)
                     '*))
         ;; TODO? Right now we just check for constants.
         ;; really, we should probably normalize those to ranges...
         (size (if (ctype:member-p sys size)
                   (let ((mems (ctype:member-members sys size)))
                     (if (and (= (length mems) 1)
                              (integerp (first mems)))
                         (first mems)
                         '*))
                   '*)))
    (ctype:single-value
     (cond ((eq etypes '*)
            (ctype:array etypes (list size) 'simple-array sys))
           ((= (length etypes) 1)
            (ctype:array (first etypes) (list size) 'simple-array sys))
           (t
            (apply #'ctype:disjoin sys
                   (loop for et in etypes
                         collect (ctype:array et (list size)
                                              'simple-array sys)))))
     sys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (17) SEQUENCES

;;; We can't simply return cons types because these functions alter them.
;;; Non-simple array types might also be an issue.
(defun type-consless-id (type sys)
  (ctype:single-value
   (if (ctype:consp type sys)
       (let ((top (ctype:top sys))) (ctype:cons top top sys))
       type)
   sys))

(define-deriver fill (sequence item &rest keys)
  (declare (ignore item keys))
  (type-consless-id sequence *clasp-system*))

(define-deriver map-into (sequence function &rest seqs)
  (declare (ignore function seqs))
  (type-consless-id sequence *clasp-system*))
(define-deriver core::map-into-sequence (result function &rest sequences)
  (declare (ignore function sequences))
  (type-consless-id result *clasp-system*))
(define-deriver core::map-into-sequence/1 (result function sequence)
  (declare (ignore function sequence))
  (type-consless-id result *clasp-system*))

(define-deriver length (sequence)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (cond ((ctype:arrayp sequence sys)
            (let ((dims (ctype:array-dimensions sequence sys)))
              (if (and (consp dims) (null (cdr dims)) (not (eq (car dims) '*)))
                  (ctype:range 'integer (car dims) (car dims) sys)
                  (ctype:range 'integer 0 (1- array-dimension-limit) sys))))
           ;; As a matter of policy we decide that LENGTH always returns a
           ;; positive fixnum. This is true of arrays and lists because there's
           ;; no way to fit 2^60 elements in memory. Technically, an extended
           ;; sequence that doesn't store everything in memory could be defined,
           ;; but this doesn't seem likely and we could find a way to ban it
           ;; outright. And should (FIXME) tell programmers not to do so.
           (t (ctype:range 'integer 0 most-positive-fixnum sys)))
     sys)))

(define-deriver sort (sequence predicate &rest keys)
  (declare (ignore predicate keys))
  (type-consless-id sequence *clasp-system*))
(define-deriver stable-sort (sequence predicate &rest keys)
  (declare (ignore predicate keys))
  (type-consless-id sequence *clasp-system*))

(define-deriver replace (seq1 seq2 &rest keys)
  (declare (ignore seq2 keys))
  (type-consless-id seq1 *clasp-system*))

(define-deriver core::concatenate-into-sequence (result &rest seqs)
  (declare (ignore seqs))
  (type-consless-id result *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (18) HASH TABLES

(define-deriver hash-table-p (object)
  (derive-type-predicate object 'core:hash-table-base *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (19) FILENAMES

(define-deriver pathnamep (object)
  (derive-type-predicate object 'pathname *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (21) STREAMS

(define-deriver streamp (object)
  (derive-type-predicate object 'stream *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (22) PRINTER

;;; WRITE et al. just return their first argument.
(define-deriver write (object &rest keys)
  (declare (ignore keys))
  (sv object))
(define-deriver prin1 (object &optional stream)
  (declare (ignore stream))
  (sv object))
(define-deriver print (object &optional stream)
  (declare (ignore stream))
  (sv object))
(define-deriver princ (object &optional stream)
  (declare (ignore stream))
  (sv object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONCURRENCY

;;; This is a KLUDGE put in so that FENCE has an identity,
;;; so that the transform (bir-to-bmir.lisp) can fire.

(define-deriver mp:fence (order)
  (declare (ignore order))
  (ctype:values nil nil (ctype:bottom *clasp-system*) *clasp-system*))

(define-deriver core:atomic-aref (order array &rest indices)
  (declare (ignore order))
  (derive-aref array indices))

(define-deriver (setf core:atomic-aref) (new order array &rest indices)
  (declare (ignore order array indices))
  (sv new))

(define-deriver core:acas (order cmp new array &rest indices)
  (declare (ignore order cmp new))
  (derive-aref array indices))
