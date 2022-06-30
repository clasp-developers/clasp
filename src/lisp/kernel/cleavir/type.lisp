(in-package #:clasp-cleavir)

(defmacro define-deriver (name deriver-name)
  `(setf (gethash ',name *derivers*) ',deriver-name))

;; Given a values ctype, returns three values:
;; 1) a list of fixed argument types
;; 2) type of any further arguments
;; 3) minimum number of arguments
(defun parse-vct (vct)
  (let* ((sys *clasp-system*)
         (req (ctype:values-required vct sys))
         (opt (ctype:values-optional vct sys))
         (rest (ctype:values-rest vct sys))
         (min (length req)))
    (values (append req opt) rest min)))

(defmethod bir-transformations:derive-return-type ((inst bir:abstract-call)
                                                   identity argstype
                                                   (system clasp))
  (let ((deriver (gethash identity *derivers*)))
    (if deriver
        (multiple-value-bind (fixed rest min) (parse-vct argstype)
          (funcall deriver fixed rest min))
        (call-next-method))))

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

(defun derive-two-arg (args rest min function &optional (default 'number))
  (declare (ignore rest))
  (ctype:single-value
   (handler-case
       (if (= min 2)
           (funcall function (first args) (second args))
           (env:parse-type-specifier default nil *clasp-system*))
     (serious-condition () (env:parse-type-specifier default nil *clasp-system*)))
   *clasp-system*))
(defun derive-two-arg-+ (args rest min) (derive-two-arg args rest min #'ty+))
(define-deriver core:two-arg-+ derive-two-arg-+)
(defun derive-two-arg-contagion (args rest min) (derive-two-arg args rest min #'ty-contagion))
(defun derive-negate (args rest min)
  (declare (ignore min))
  (ctype:single-value
   (ty-negate (or (first args) (first rest)))
   *clasp-system*))
(define-deriver core:negate derive-negate)
(defun derive-two-arg-- (args rest min)
  (declare (ignore rest))
  (ctype:single-value
   (if (= min 2)
       (ty+ (first args) (ty-negate (second args)))
       (env:parse-type-specifier 'number nil *clasp-system*))
   *clasp-system*))
(define-deriver core:two-arg-- derive-two-arg--)
;;; This is imprecise, e.g. a rational*integer can be an integer, but should
;;; still be sound.
(define-deriver core:two-arg-* derive-two-arg-contagion)
;;; Can't use contagion for / since e.g. integer/integer is a ratio.

(defun derive-monotonic1 (args rest min function &key (inf '*) (sup '*))
  (declare (ignore min))
  (let ((ty (or (first args) rest)))
    (ty-irrat-monotonic1 ty function :inf inf :sup sup)))
(defun derive-exp (args rest min) (derive-monotonic1 args rest min #'exp :inf 0f0))
(define-deriver exp derive-exp)

(defun derive-sqrt (args rest min)
  (declare (ignore min))
  (let ((ty (or (first args) rest)) (sys *clasp-system*))
    (if (ctype:rangep ty sys)
        (let ((low (ctype:range-low ty sys)))
          (if (and low (>= low 0)) ; no complex results
              (ty-irrat-monotonic1 ty #'sqrt :inf 0f0)
              (ctype:single-value (env:parse-type-specifier 'number nil sys) sys)))
        (ctype:single-value (env:parse-type-specifier 'number nil sys) sys))))
(define-deriver sqrt derive-sqrt)

;;; If the argument is a real, return [-1,1]. otherwise just NUMBER
;;; Technically the range could be reduced sometimes, but figuring out the
;;; exact values is kind of a pain and it doesn't seem that useful.
(defun derive-sincos (args rest min)
  (declare (ignore min))
  (let ((ty (or (first args) rest)) (sys *clasp-system*))
    (ctype:single-value
     (if (ctype:rangep ty sys)
         (let ((kind (ctype:range-kind ty sys)))
           (when (member kind '(integer rational real))
             (setf kind 'single-float))
           (ctype:range kind (coerce -1 kind) (coerce 1 kind) sys))
         (env:parse-type-specifier 'number nil sys))
     sys)))
(define-deriver sin derive-sincos)
(define-deriver cos derive-sincos)

(defun derive-tanh (args rest min) (derive-monotonic1 args rest min #'tanh :inf -1f0 :sup 1f0))
(define-deriver tanh derive-tanh)

(defun derive-ash (args rest min)
  (derive-two-arg args rest min #'ty-ash 'integer))
(define-deriver ash derive-ash)

(defun derive-float (args rest min)
  (let* ((sys *clasp-system*)
         ;; We only really care about the first two arguments, since anything
         ;; more will be an error. So we just pop the rest type on the end.
         (args (append args (list rest)))
         (float (env:parse-type-specifier 'float nil sys))
         (rat (env:parse-type-specifier 'rational nil sys))
         #+(or) ; nonexistent
         (short (ctype:range 'short-float '* '* sys))
         (single (ctype:range 'single-float '* '* sys))
         (double (ctype:range 'double-float '* '* sys))
         #+(or) ; nonexistent
         (long (ctype:range 'long-float '* '* *clasp-system*)))
    ;; FIXME: More sophisticated type operations would make this more
    ;; precise. For example, it would be good to derive that if the
    ;; argument is an (or single-float rational), the result is a
    ;; single float.
    (ctype:single-value
     (cond ((> min 1)
            (let ((proto (second args)))
              (cond #+(or)((arg-subtypep proto short) short)
                    ((ctype:subtypep proto single sys) single)
                    ((ctype:subtypep proto double sys) double)
                    #+(or)((arg-subtypep proto long) long)
                    (t float))))
           ((and (= min 1)
                 (ctype:bottom-p (second args) sys))
            (let ((arg (first args)))
              (cond ((ctype:subtypep arg float sys) arg)
                    ((ctype:subtypep arg rat sys) single)
                    (t float))))
           (t float))
     sys)))

(define-deriver float derive-float)

(defun derive-random (args rest min)
  (declare (ignore min))
  (let* ((sys *clasp-system*)
         (max (or (first args) rest)))
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
(define-deriver random derive-random)

(defun derive-aref (args rest min)
  (declare (ignore min))
  (let ((sys *clasp-system*)
        (ct (if (null args) rest (first args))))
    (ctype:single-value
     (if (and (consp ct)
              (member (first ct) '(array simple-array vector))
              (consp (cdr ct)))
         (second ct)
         (ctype:top sys))
     sys)))

(define-deriver aref derive-aref)
(define-deriver row-major-aref derive-aref)

(macrolet ((def (fname etype)
             (let ((derivename
                     (make-symbol (concatenate 'string
                                               (symbol-name '#:derive-)
                                               (symbol-name fname)))))
               `(progn
                  (defun ,derivename (args rest min)
                    (declare (ignore args rest min))
                    (let ((sys *clasp-system*))
                      (ctype:single-value
                       (ctype:array ',etype '(*) 'simple-array sys)
                       sys)))
                  (define-deriver ,fname ,derivename)))))
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

(defun derive-cons (args rest min)
  (declare (ignore args rest min))
  ;; We can't forward the argument types into the cons type, since we don't
  ;; know if this cons will be mutated. So we just return the CONS type.
  ;; This is useful so that the compiler understands that CONS definitely
  ;; returns a CONS and it does not need to insert any runtime checks.
  (let* ((sys clasp-cleavir:*clasp-system*) (top (ctype:top sys)))
    (ctype:single-value (ctype:cons top top sys) sys)))
(define-deriver cons derive-cons)

(defun derive-list (args rest min)
  (let* ((sys clasp-cleavir:*clasp-system*) (top (ctype:top sys)))
    (ctype:single-value
     (cond ((> min 0) (ctype:cons top top sys))
           ((and (= min 0) (null args) (ctype:bottom-p rest sys))
            (ctype:member sys nil))
           (t (ctype:disjoin sys (ctype:member sys nil)
                             (ctype:cons top top sys))))
     sys)))
(define-deriver list derive-list)

(defun derive-list* (args rest min)
  (let* ((sys clasp-cleavir:*clasp-system*) (top (ctype:top sys)))
    (ctype:single-value
     (cond ((> min 1) (ctype:cons top top sys))
           ((and (= min 1) (null (rest args)) (ctype:bottom-p rest sys))
            (first args))
           (t
            (ctype:disjoin sys (if args (first args) rest)
                           (ctype:cons top top sys))))
     sys)))
(define-deriver list* derive-list*)

;;; WRITE et al. just return their first argument.
(defun derive-write (args rest min)
  (declare (ignore min))
  (ctype:single-value (or (first args) rest) *clasp-system*))
(define-deriver write derive-write)
(define-deriver prin1 derive-write)
(define-deriver print derive-write)
(define-deriver princ derive-write)
