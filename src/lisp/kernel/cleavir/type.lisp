(in-package #:clasp-cleavir)

(defmacro with-types (lambda-list argstype (&key default) &body body)
  `(domain:with-info-type (*clasp-system* ,lambda-list ,default)
                          domain:type ,argstype
     ,@body))

(defmacro with-deriver-types (lambda-list argstype &body body)
  `(with-types ,lambda-list ,argstype
     (:default (ctype:values-bottom *clasp-system*))
     ,@body))

;;; Define a type deriver function. See domain:deriver-lambda for details.
;;; The main thing is that -p arguments in &key and &optional are a bit different.
;;; T means that the argument is definitely provided. NIL means it may or may not
;;; be provided. To check if it's actually not provided, check against the default
;;; type, which had better be NIL if you need to know that.
(defmacro define-deriver (name lambda-list &body body)
  (let* ((fname (make-symbol (format nil "~a-DERIVER" (write-to-string name))))
         (as (gensym "ARGSTYPE")))
    `(progn
       (defun ,fname (,as)
         (block ,(core:function-block-name name)
           (with-deriver-types ,lambda-list ,as ,@body)))
       (setf (gethash ',name *derivers*) ',fname)
       ',name)))

(defmacro import-deriver (name &optional (from name))
  `(progn
     (setf (gethash ',name *derivers*)
           (let ((deriver (cleavir-derive-cl:deriver domain:type ',from)))
             (lambda (argstype)
               (funcall deriver *clasp-system* domain:type argstype))))
     ',name))
(defmacro import-derivers (&rest names)
  `(progn
     ,@(loop for name in names
             collect `(import-deriver ',name))))

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
                      :origin (bir:origin inst)
                      :fname identity :condition e)
            (call-next-method)))
        (call-next-method))))

(defun sv (type) (ctype:single-value type *clasp-system*))

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

;;; If TYPE is an EQL type, return its value and T, otherwise NIL and NIL.
(defun type-constant-value (type sys)
  (if (ctype:member-p sys type)
      (let ((members (ctype:member-members sys type)))
        (if (= (length members) 1)
            (values (first members) t)
            (values nil nil)))
      (values nil nil)))

;;; Reduce a type to a single interval. This is general worse than the original
;;; type, e.g. (or (integer 0 4) (integer 12 19)) would become (integer 0 19),
;;; but single intervals are way easier to work with.
(defun type-approximate-interval (type sys)
  (cond ((ctype:rangep type sys)
         (range->interval type sys))
        ((ctype:disjunctionp type sys)
         (reduce #'interval-merge
                 (loop for st in (ctype:disjunction-ctypes type sys)
                        collect (type-approximate-interval st sys))
                 :initial-value (make-empty-interval)))
        ((ctype:conjunctionp type sys)
         (reduce #'interval-intersect
                 (loop for st in (ctype:conjunction-ctypes type sys)
                       collect (type-approximate-interval st sys))
                 :initial-value (make-unbounded-interval)))
        (t (make-unbounded-interval)))) ; unknown

(defmethod cleavir-derive-cl:generalized-true ((client clasp) operator)
  (declare (ignore operator))
  (ctype:member client t))
(defmethod cleavir-derive-cl:generalized-boolean ((client clasp) operator)
  (declare (ignore operator))
  (ctype:member client t nil))

(defmethod cleavir-derive-cl:simple-arrays-actually-adjustable-p ((client clasp))
  nil)
(defmethod cleavir-derive-cl:array-dimension-limit-value ((client clasp))
  array-dimension-limit)
(defmethod cleavir-derive-cl:array-rank-limit-value ((client clasp))
  array-rank-limit)
(defmethod cleavir-derive-cl:maximum-list-length ((client clasp))
  most-positive-fixnum)
(defmethod cleavir-derive-cl:maximum-sequence-length ((client clasp))
  most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (4) TYPES AND CLASSES

(define-deriver typep (obj type &optional env)
  (declare (ignore env))
  (let ((sys *clasp-system*))
    (multiple-value-bind (typec validp) (type-constant-value type sys)
      (if validp
          (derive-type-predicate obj typec sys)
          (ctype:single-value (ctype:member sys t nil) sys)))))

(define-deriver core::headerp (obj type)
  (let ((sys *clasp-system*))
    (multiple-value-bind (typec validp) (type-constant-value type sys)
      (if validp
          (derive-type-predicate obj typec sys)
          (ctype:single-value (ctype:member sys t nil) sys)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (5) DATA AND CONTROL FLOW

(define-deriver functionp (obj)
  (derive-type-predicate obj 'function *clasp-system*))
(define-deriver compiled-function-p (obj)
  (derive-type-predicate obj 'compiled-function *clasp-system*))

(import-derivers eq eql)
(import-derivers identity values values-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (8) STRUCTURES

(import-deriver copy-structure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (9) CONDITIONS

(import-deriver error)

(define-deriver core::etypecase-error (datum types)
  (declare (ignore datum types))
  (ctype:values-bottom *clasp-system*))

(import-derivers cerror signal warn invoke-debugger break)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (10) SYMBOLS

(define-deriver symbolp (object)
  (derive-type-predicate object 'symbol *clasp-system*))
(define-deriver keywordp (object)
  (derive-type-predicate object 'keyword *clasp-system*))

(import-derivers symbol-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (11) PACKAGES

(define-deriver packagep (object)
  (derive-type-predicate object 'package *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (12) NUMBERS

(import-derivers numberp complexp realp rationalp floatp integerp
                 random-state-p)
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
(define-deriver core:fixnump (object)
  (derive-type-predicate object 'fixnum *clasp-system*))

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

(import-derivers truncate floor ceiling)

(import-derivers mod rem)

(import-derivers ffloor fceiling ftruncate)

(import-deriver core:two-arg-+ +)
(import-deriver core:negate -)
(import-deriver core:two-arg-- -)

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

(import-deriver core:two-arg-* *)
(import-deriver core:two-arg-/ /)
(import-deriver core:reciprocal /)

(import-deriver exp)
(import-deriver expt)

(import-deriver sqrt)
(import-deriver log)

(import-derivers sin cos tan)

(import-derivers asin acos)

(import-derivers sinh cosh tanh asinh acosh atanh)
(import-deriver abs)

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

(import-deriver random)

(import-derivers logcount integer-length)
(import-deriver lognot)

(import-derivers logand logior logxor logandc1 logandc2 logorc1 logorc2
                 logeqv lognand lognor)
(import-deriver core:logand-2op logand)
(import-deriver core:logior-2op logior)
(import-deriver core:logxor-2op logxor)
(import-deriver core:logeqv-2op logeqv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (13) CHARACTERS

(import-derivers characterp standard-char-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (14) CONSES

(import-deriver cons)
(import-derivers consp atom listp)

(import-derivers car cdr caar cadr cdar cddr
                 caaar caadr cadar caddr
                 caaaar caaadr caadar caaddr
                 cadaar cadadr caddar cadddr
                 cdaaar cdaadr cdadar cdaddr
                 cddaar cddadr cdddar cddddr
                 rest first second third fourth fifth sixth seventh
                 eighth ninth tenth
                 rplaca rplacd)

(import-derivers list list*)
(import-derivers endp null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (15) ARRAYS

(define-deriver make-array (dimensions
                            &key (element-type '(eql t))
                            initial-element initial-contents
                            (adjustable '(eql nil))
                            (fill-pointer '(eql nil)) (displaced-to '(eql nil))
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

(import-derivers aref (setf aref))
(import-deriver core:vref aref)
(import-deriver (setf core:vref) (setf aref))

(import-deriver array-rank)
(import-deriver arrayp)

(import-derivers row-major-aref (setf row-major-aref))

(import-derivers vectorp bit-vector-p simple-bit-vector-p)

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

(import-derivers simple-string-p string)

(define-deriver make-string (size &key (initial-element 'character)
                                  (element-type '(eql character)))
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

(import-derivers concatenate copy-seq elt fill make-sequence subseq
                 map map-into merge reduce count count-if count-if-not
                 length reverse nreverse sort stable-sort find find-if find-if-not
                 position position-if position-if-not search mismatch
                 replace substitute nsubstitute
                 substitute-if substitute-if-not nsubstitute-if nsubstitute-if-not
                 remove delete remove-if remove-if-not delete-if delete-if-not
                 remove-duplicates delete-duplicates)
;;; We can't simply return cons types because these functions alter them.
;;; Non-simple array types might also be an issue.
(defun type-consless-id (type sys)
  (ctype:single-value
   (if (ctype:consp type sys)
       (let ((top (ctype:top sys))) (ctype:cons top top sys))
       type)
   sys))

(define-deriver core::map-into-sequence (result function &rest sequences)
  (declare (ignore function sequences))
  (type-consless-id result *clasp-system*))
(define-deriver core::map-into-sequence/1 (result function sequence)
  (declare (ignore function sequence))
  (type-consless-id result *clasp-system*))

(define-deriver core::concatenate-into-sequence (result &rest seqs)
  (declare (ignore seqs))
  (type-consless-id result *clasp-system*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (18) HASH TABLES

(import-deriver hash-table-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (19) FILENAMES

(import-deriver pathnamep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (21) STREAMS

(import-deriver streamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (22) PRINTER

(import-derivers write prin1 print princ pprint)

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
