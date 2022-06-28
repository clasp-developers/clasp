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

(defmacro with-deriver-types (lambda-list argstype &body body)
  (multiple-value-bind (req opt rest keyp keys)
      (core:process-lambda-list lambda-list 'function)
    ;; TODO: &allow-other-keys
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
                 (ctype:values-bottom ,gsys)
                 ;; Valid call
                 (let* (,@(loop for reqv in (rest req)
                                collect `(,reqv (or (pop ,greq)
                                                    (pop ,gopt) ,grest)))
                        ,@(loop for (optv _ -p) on (rest opt) by #'cdddr
                                when -p
                                  collect `(,-p (cond (,greq t)
                                                      ((or ,gopt ,grb) :maybe)
                                                      (t nil)))
                                collect `(,optv (or (pop ,greq)
                                                    (pop ,gopt)
                                                    ,grest)))
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
                                          (ecase ,r-p
                                            ((nil)
                                             (env:parse-type-specifier
                                              ',r-def nil ,gsys))
                                            ((t)
                                             (kwarg-type ',kw ,greq ,gopt ,grest
                                                         ,gsys))
                                            ((:maybe)
                                             (ctype:disjoin
                                              ,gsys
                                              (env:parse-type-specifier
                                               ',r-def nil ,gsys)
                                              (kwarg-type ',kw ,greq ,gopt ,grest
                                                          ,gsys)))))))
                   ,@body)))))))

;;; Lambda lists are basically ordinary lambda lists, but without &aux
;;; because &aux sucks.
;;; &optional defaults are ignored.
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
  (let* ((fname (make-symbol (format nil "~a-DERIVER" (symbol-name name))))
         (as (gensym "ARGSTYPE")))
    `(progn
       (defun ,fname (,as)
         (block ,name
           (with-deriver-types ,lambda-list ,as ,@body)))
       (setf (gethash ',name *derivers*) ',fname)
       ',name)))

(defmethod bir-transformations:derive-return-type ((inst bir:abstract-call)
                                                   identity argstype
                                                   (system clasp))
  (let ((deriver (gethash identity *derivers*)))
    (if deriver
        (funcall deriver argstype)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (12) NUMBERS

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
       ((integer ratio rational short-float) 'single-float)
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
       ((integer ratio rational short-float single-float long-float) 'double-float)
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
                           (hxp (list high))
                           (t high))
                     (cond ((null low) '*)
                           (lxp (list low))
                           (t low))
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
                                        (t inf)))
                                 (lxp (list (funcall function low)))
                                 (t (funcall function low))))
                     (ohigh (cond ((not high)
                                   (cond ((not (numberp sup)) sup)
                                         ((eq kind 'double-float) (float sup 0d0))
                                         (t sup)))
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



(define-deriver core:two-arg-+ (n1 n2) (sv (ty+ n1 n2)))
(define-deriver core:negate (arg) (sv (ty-negate arg)))
(define-deriver core:two-arg-- (n1 n2) (sv (ty+ n1 (ty-negate n2))))
;;; This is imprecise, e.g. a rational*integer can be an integer, but should
;;; still be sound.
(define-deriver core:two-arg-* (n1 n2) (sv (ty-contagion n1 n2)))
(define-deriver core:two-arg-/ (n1 n2) (sv (ty-divcontagion n1 n2)))

(define-deriver exp (arg) (ty-irrat-monotonic1 arg #'exp :inf 0f0))

(define-deriver sqrt (arg)
  (let ((sys *clasp-system*))
    (if (ctype:rangep arg sys)
        (let ((low (ctype:range-low arg sys)))
          (if (and low (>= low 0)) ; no complex results
              (ty-irrat-monotonic1 arg #'sqrt :inf 0f0)
              (ctype:single-value
               (env:parse-type-specifier 'number nil sys) sys)))
        (ctype:single-value (env:parse-type-specifier 'number nil sys) sys))))

;;; If the argument is a real, return [-1,1]. otherwise just NUMBER
;;; Technically the range could be reduced sometimes, but figuring out the
;;; exact values is kind of a pain and it doesn't seem that useful.
(defun derive-sincos (arg)
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep arg sys)
         (let ((kind (ctype:range-kind arg sys)))
           (when (member kind '(integer rational real))
             (setf kind 'single-float))
           (ctype:range kind (coerce -1 kind) (coerce 1 kind) sys))
         (env:parse-type-specifier 'number nil sys))
     sys)))
(define-deriver sin (arg) (derive-sincos arg))
(define-deriver cos (arg) (derive-sincos arg))

(define-deriver tanh (arg) (ty-irrat-monotonic1 arg #'tanh :inf -1f0 :sup 1f0))

(define-deriver ash (num shift) (sv (ty-ash num shift)))

(define-deriver float (num &optional (proto nil protop))
  ;; FIXME: More sophisticated type operations would make this more
  ;; precise. For example, it would be good to derive that if the
  ;; argument is an (or single-float rational), the result is a
  ;; single float.
  (let* ((sys *clasp-system*)
         (float (env:parse-type-specifier 'float nil sys))
         (rat (env:parse-type-specifier 'rational nil sys))
         (single (env:parse-type-specifier 'single-float nil sys))
         (double (env:parse-type-specifier 'double-float nil sys)))
    (flet ((float1 ()
             (cond ((ctype:subtypep num float sys) num) ; no coercion
                   ((ctype:subtypep num rat sys) single)
                   (t float)))
           (float2 ()
             (cond #+(or)((arg-subtypep proto short) short)
                   ((ctype:subtypep proto single sys) single)
                   ((ctype:subtypep proto double sys) double)
                   #+(or)((arg-subtypep proto long) long)
                   (t float))))
      (ctype:single-value
       (cond ((eq protop t) (float2)) ; definitely supplied
             ((eq protop :maybe)
              (ctype:disjoin sys (float1) (float2)))
             (t (float1)))
       sys))))

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
           (t (env:parse-type-specifier '(real 0) nil sys)))
     sys)))

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

(defun derive-aref (array indices)
  (declare (ignore indices))
  (let ((sys *clasp-system*))
    (ctype:single-value
     (if (and (consp array)
              (member (first array) '(array simple-array vector))
              (consp (cdr array)))
         (second array)
         (ctype:top sys))
     sys)))

(define-deriver aref (array &rest indices) (derive-aref array indices))

(define-deriver arrayp (object)
  (derive-type-predicate object 'array *clasp-system*))

(define-deriver row-major-aref (array index) (derive-aref array index))

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
