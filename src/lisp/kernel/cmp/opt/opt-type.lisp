(in-package #:core)

;;;; Optimizations for Chapter 4, Types and Classes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEP
;;;

;;; A word on efficiency. Throughout this code, we try to expand directly into
;;; TYPEQ when possible. For example we want (if (typep x 'fixnum) ...) to
;;; expand into (if (typeq x fixnum) ...). The compiler (both bclasp and cclasp)
;;; will have an easier time dealing with typeq when it's used directly as an
;;; if conditional, and in bclasp's case this is the only way to avoid clumsily
;;; returning a T or NIL and then testing on that at runtime.
;;; Whenever we expand into more code, we declare (speed (safety 0)). This is to
;;; tell cclasp not to insert type checks into our type checks. E.g., if we
;;; expand (typep x '(integer 0 7)) into (if (typeq x fixnum) (<= 0 x 7) nil),
;;; we want to use inline fixnum arithmetic without x being checked within the
;;; <=, since we just did that check.

;;; TODO: Treat malformed types better:
;;; We should DEFINITELY NOT cause compile time errors
;;; We COULD signal warnings on types like (standard-char foo)

;;; Array type map.
;;; The array hierarchy is defined in array.h. Basically here's how it goes:
;;; Arrays are divided by element type, and then in two other ways:
;;; Being a vector or not, and being simple or not.
;;; This means four array classes per element type.
;;; These tables map element types to those classes.
(macrolet ((def (name map-name untyped &rest types)
             `(progn
                (defparameter ,map-name
                  ',(loop for element-type in
                          '(bit fixnum ext:byte2 ext:byte4 ext:byte8
                            ext:byte16 ext:byte32 ext:byte64 ext:integer2
                            ext:integer4 ext:integer8 ext:integer16
                            ext:integer32 ext:integer64
                            single-float double-float base-char character t)
                          for uaet in types
                          collect (cons element-type uaet)))
                (defun ,name (uaet)
                  (if (eq uaet '*)
                      ',untyped
                      (let ((pair (assoc uaet ,map-name)))
                        (if pair
                            (cdr pair)
                            (error "BUG: Unknown UAET ~a in ~a" uaet ',name))))))))
  (def simple-vector-type +simple-vector-type-map+ core:abstract-simple-vector
    simple-bit-vector
    core:simple-vector-fixnum
    core:simple-vector-byte2-t
    core:simple-vector-byte4-t
    core:simple-vector-byte8-t
    core:simple-vector-byte16-t
    core:simple-vector-byte32-t
    core:simple-vector-byte64-t
    core:simple-vector-int2-t
    core:simple-vector-int4-t
    core:simple-vector-int8-t
    core:simple-vector-int16-t
    core:simple-vector-int32-t
    core:simple-vector-int64-t
    core:simple-vector-float
    core:simple-vector-double
    simple-base-string
    core:simple-character-string
    simple-vector)
  (def complex-vector-type +complex-vector-type-map+ core:complex-vector
    core:bit-vector-ns
    core:complex-vector-fixnum
    core:complex-vector-byte2-t
    core:complex-vector-byte4-t
    core:complex-vector-byte8-t
    core:complex-vector-byte16-t
    core:complex-vector-byte32-t
    core:complex-vector-byte64-t
    core:complex-vector-int2-t
    core:complex-vector-int4-t
    core:complex-vector-int8-t
    core:complex-vector-int16-t
    core:complex-vector-int32-t
    core:complex-vector-int64-t
    core:complex-vector-float
    core:complex-vector-double
    core:str8ns
    core:str-wns
    core:complex-vector-t)
  (def simple-mdarray-type +simple-mdarray-type-map+ core:simple-mdarray
    core:simple-mdarray-bit
    core:simple-mdarray-fixnum
    core:simple-mdarray-byte2-t
    core:simple-mdarray-byte4-t
    core:simple-mdarray-byte8-t
    core:simple-mdarray-byte16-t
    core:simple-mdarray-byte32-t
    core:simple-mdarray-byte64-t
    core:simple-mdarray-int2-t
    core:simple-mdarray-int4-t
    core:simple-mdarray-int8-t
    core:simple-mdarray-int16-t
    core:simple-mdarray-int32-t
    core:simple-mdarray-int64-t
    core:simple-mdarray-float
    core:simple-mdarray-double
    core:simple-mdarray-base-char
    core:simple-mdarray-character
    core:simple-mdarray-t)
  (def complex-mdarray-type +complex-mdarray-type-map+ core:mdarray
    core:mdarray-bit
    core:mdarray-fixnum
    core:mdarray-byte2-t
    core:mdarray-byte4-t
    core:mdarray-byte8-t
    core:mdarray-byte16-t
    core:mdarray-byte32-t
    core:mdarray-byte64-t
    core:mdarray-int2-t
    core:mdarray-int4-t
    core:mdarray-int8-t
    core:mdarray-int16-t
    core:mdarray-int32-t
    core:mdarray-int64-t
    core:mdarray-float
    core:mdarray-double
    core:mdarray-base-char
    core:mdarray-character
    core:mdarray-t))

(defun array-typep-form (object simplicity dims uaet)
  (let ((rank (if (eq dims '*) '* (length dims)))
        (simple-vector-type (simple-vector-type uaet))
        (complex-vector-type (complex-vector-type uaet))
        (simple-mdarray-type (simple-mdarray-type uaet))
        ;; NOTE: At the moment the "complex-mdarray" classes
        ;; are actually simple+complex, making the below tests
        ;; a little bit redundant.
        (mdarray-type (complex-mdarray-type uaet)))
    (flet ((generate-test (simple complex if-simple if-complex if-no)
             (ecase simplicity
               ((simple-array)
                `(if (cleavir-primop:typeq object ,simple)
                     ,if-simple ,if-no))
               ((complex-array)
                ;; KLUDGE: Because simple-mdarray is a subtype of mdarray,
                ;; to test for complex arrays we have to actually rule out
                ;; the simple. This means we do a redudant test for vectors,
                ;; though, since this confusion isn't in place for the
                ;; vector classes.
                `(if (cleavir-primop:typeq object ,simple)
                     nil
                     (if (cleavir-primop:typeq object ,complex)
                         ,if-complex ,if-no)))
               ((array)
                `(if (cleavir-primop:typeq object ,simple)
                     ,if-simple
                     (if (cleavir-primop:typeq object ,complex)
                         ,if-complex ,if-no))))))
      (case rank
        ((1) ; vector
         (let ((length (first dims)))
           (cond
             ((not (eq length '*)) ; have to check length.
              `(let ((object ,object))
                 (declare (optimize speed (safety 0)))
                 ,(generate-test simple-vector-type complex-vector-type
                                 ;; This LENGTH can be inlined; see cleavir/bir-to-bmir
                                 `(eq (length (the ,simple-vector-type object))
                                      ',length)
                                 `(eq (length object) ',length)
                                 'nil)))
             ((eq simplicity 'simple-array)
              ;; expand directly into typeq to ease compiler work.
              `(cleavir-primop:typeq ,object ,simple-vector-type))
             (t
              `(let ((object ,object))
                 (declare (optimize speed (safety 0)))
                 ,(generate-test simple-vector-type complex-vector-type
                                 't 't 'nil))))))
        ((*) ; anything, and dimensions are unspecified
         ;; for general arrays we have superclasses to use
         (if (and (eq uaet '*) (eq simplicity 'array))
             `(cleavir-primop:typeq ,object array)
             `(let ((object ,object))
                (declare (optimize speed (safety 0)))
                ,(generate-test simple-vector-type complex-vector-type
                                't 't
                                (generate-test simple-mdarray-type
                                               mdarray-type
                                               't 't 'nil)))))
        (otherwise ; an mdarray with possibly specified dimensions
         `(let ((object ,object))
            (declare (optimize speed (safety 0)))
            (block nil
              ;; We use a block so the dimensions check code is only
              ;; generated once.
              ;; First, if it's not an mdarray, return NIL early.
              ,(generate-test simple-mdarray-type mdarray-type
                              nil nil '(return nil))
              ;; Now, it is an mdarray, so check dimensions.
              (and
               ;; see transform in cleavir/bir-to-bmir.lisp
               (= (array-rank (the (and array (not (simple-array * (*)))) object))
                  ',rank)
               ,@(loop for dim in dims
                       for i from 0
                       unless (eq dim '*)
                         collect `(eq (array-dimension object ',i) ,dim))))))))))

(defun cons-typep-form (object cart cdrt env)
  ;; If the cart and cdrt are both *, expand directly into typeq.
  (if (and (eq cart '*) (eq cdrt '*))
      `(cleavir-primop:typeq ,object cons)
      ;; Otherwise...
      `(let ((object ,object))
         (declare (optimize speed (safety 0)))
         (if (cleavir-primop:typeq object cons)
             (and ,@(if (eq cart '*)
                        nil
                        (list
                         (typep-expansion '(cleavir-primop:car object)
                                          cart env)))
                  ,@(if (eq cdrt '*)
                        nil
                        (list
                         (typep-expansion '(cleavir-primop:cdr object)
                                          cdrt env))))
             nil))))

(defun valid-number-type-p (head low high)
  (and (or (eq low '*)
           ;; No recursive expansion even if we had a constant head,
           ;; since integer => (integer * *) and we checked * already.
           (typep low head)
           (and (consp low)
                (null (cdr low))
                (typep (car low) head)))
       (or (eq high '*)
           (typep high head)
           (and (consp high)
                (null (cdr high))
                (typep (car high) head)))))

(defun fixnum-<=-operator ()
  ;; In cclasp, rely on the compiler to reduce to a fixnum operator-
  ;; this lets it work more cleanly as the frontend doesn't need to know
  ;; about primops.
  ;; In bclasp, do it straight.
  (if cmp:*cleavir-compile-hook*
      '<=
      'cleavir-primop:fixnum-not-greater))

;;; This is more complicated than for the other real types because of
;;; the fixnum/bignum split.
;;; What we basically arrange is to use fixnum arithmetic when possible.
(defun integral-interval-typep-form (object low high)
  ;; Turn exclusive ranges into inclusive ones.
  (when (consp low) (setf low (1+ (car low))))
  (when (consp high) (setf high (1- (car high))))
  (cond ((and (eql low most-negative-fixnum)
              (eql high most-positive-fixnum))
         ;; type is just fixnum
         `(cleavir-primop:typeq ,object fixnum))
        ((and (not (eql low '*)) (>= low most-negative-fixnum)
              (not (eql high '*)) (<= high most-positive-fixnum))
         ;; type only includes fixnums
         `(let ((object ,object))
            (declare (optimize speed (safety 0)))
            (and (cleavir-primop:typeq object fixnum)
                 ,@(unless (eql low most-negative-fixnum)
                     `((,(fixnum-<=-operator)
                        ,low (the (values fixnum &rest nil) object))))
                 ,@(unless (eql high most-positive-fixnum)
                     `((,(fixnum-<=-operator)
                        (the (values fixnum &rest nil) object) ,high))))))
        (t ; general case
         `(let ((object ,object))
            (declare (optimize speed (safety 0)))
            (if (cleavir-primop:typeq object fixnum)
                (and
                 ,@(unless (or (eql low '*)
                               (<= low most-negative-fixnum))
                     `((,(fixnum-<=-operator)
                        ,low (the (values fixnum &rest nil) object))))
                 ,@(unless (or (eql high '*)
                               (>= high most-positive-fixnum))
                     `((,(fixnum-<=-operator)
                        (the (values fixnum &rest nil) object) ,high))))
                (if (cleavir-primop:typeq object bignum)
                    (and
                     ,@(unless (eql low '*) `((<= ,low object)))
                     ,@(unless (eql high '*) `((<= object ,high))))
                    nil))))))

;;; The simpler version
;;; FIXME: Use floating point compares, etc, when available
(defun real-interval-test (oform low high)
  `(and ,@(cond ((eq high '*) nil)
                ((consp high) `((< ,oform ,(car high))))
                (t `((<= ,oform ,high))))
        ,@(cond ((eq low '*) nil)
                ((consp low) `((> ,oform ,(car low))))
                (t `((>= ,oform ,low))))))

(defun real-interval-typep-form (object head low high)
  (ecase head
    ((integer) (integral-interval-typep-form object low high))
    ((rational)
     `(let ((object ,object))
        (declare (optimize speed (safety 0)))
        (or ,(integral-interval-typep-form 'object low high)
            (if (cleavir-primop:typeq object ratio)
                ,(real-interval-test `(the ratio object) low high)
                nil))))
    ((short-float single-float double-float long-float)
     (if (and (eql low '*) (eql high '*))
         `(cleavir-primop:typeq ,object ,head)
         `(let ((object ,object))
            (declare (optimize speed (safety 0)))
            (if (cleavir-primop:typeq object ,head)
                ,(real-interval-test `(the (values ,head &rest nil) object) low high)
                nil))))
    ((float)
     ;; only singles and doubles actually exist.
     ;; FIXME: write in this assumption better in case we change it later.
     `(let ((object ,object))
        (declare (optimize speed (safety 0)))
         (if (if (cleavir-primop:typeq object single-float)
                 t
                 (if (cleavir-primop:typeq object double-float) t nil))
             ,(real-interval-test `(the float object) low high)
             nil)))
    ((real)
     `(let ((object ,object))
        (declare (optimize speed (safety 0)))
        (or ,(integral-interval-typep-form 'object low high)
            (if (if (cleavir-primop:typeq object single-float)
                    t
                    (if (cleavir-primop:typeq object double-float)
                        t
                        (if (cleavir-primop:typeq object ratio) t nil)))
                ,(real-interval-test '(the (values real &rest nil) object) low high)
                nil))))))

(defun typep-expansion (object type env &optional (form nil formp))
  (ext:with-current-source-form (type)
    (flet ((default () (if formp form `(typep object ',type)))
           (recur (type) (typep-expansion 'object type env)))
      (multiple-value-bind (head args) (normalize-type type)
        (case head
          ;; We do this expansion so that the object is used.
          ;; This prevents the compiler from complaining on code like
          ;; (lambda (x) (typep x t)) which is occasionally generated.
          ;; And more importantly that any side effects in the object
          ;; form are executed.
          ((t) `(progn ,object t))
          ((nil) `(progn ,object nil))
          ((and or)
           `(let ((object ,object)) ; prevent multiple evaluation
              (,head ,@(mapcar #'recur args))))
          ((not) `(not ,(typep-expansion object (first args) env)))
          ((eql) `(eql ,object ',(first args)))
          ((satisfies)
           `(if (funcall (fdefinition ',(first args)) ,object) t nil))
          ((member)
           `(let ((object ,object))
              (or ,@(mapcar (lambda (x) `(eql object ',x)) args))))
          ((cons)
           (destructuring-bind (&optional (cart '*) (cdrt '*)) args
             (cons-typep-form object cart cdrt env)))
          ((simple-array complex-array array)
           (destructuring-bind (&optional (et '*) (dims '*)) args
             (array-typep-form
              object head
              (if (integerp dims)
                  (make-list dims :initial-element '*)
                  dims)
              (if (eq et '*)
                  et
                  (upgraded-array-element-type et env)))))
          ((sequence)
           `(let ((object ,object))
              (or (listp object) (vectorp object))))
          ((standard-char)
           `(let ((object ,object))
              (and (characterp object) (standard-char-p object))))
          ;; NOTE: Probably won't actually occur, due to normalization.
          ((bignum)
           `(let ((object ,object))
              (and (integerp object) (not (fixnump object)))))
          ((#+short-float short-float #+long-float long-float
            single-float double-float
            float integer rational real)
           (destructuring-bind (&optional (low '*) (high '*)) args
             ;; Check the bounds for correctness.
             ;; We use primitives for testing with them, so we want
             ;; to be sure that they're valid.
             (cond ((valid-number-type-p head low high)
                    (real-interval-typep-form object head low high))
                   (t (cmp:warn-invalid-number-type nil type)
                      (default)))))
          ((complex)
           ;; This covers (complex whatever) types in addition to just complex.
           ;; We don't have multiple complex types in the backend,
           ;; so we can just do this.
           ;; See comment in DEFUN TYPEP in lsp/predlib.lisp.
           `(complexp ,object))
          (otherwise
           ;; Last ditch efforts.
           (cond
             ;; Is there a simple predicate?
             ((and (null args) (simple-type-predicate head))
              `(,(simple-type-predicate head) ,object))
             ;; Could be a C++ class kind of thing.
             ;; NOTE: This is a static header check, so it shouldn't be used
             ;; for anything that could be subclassed. The most likely candidate
             ;; for this problem is STREAM, but it's caught by the previous case.
             ((and (null args) (gethash head core:+type-header-value-map+))
              `(cleavir-primop:typeq ,object ,type))
             ;; Maybe it's a class name? (See also, comment in clos/defclass.lisp.)
             ((and (null args) (symbolp head) (class-info head env))
              ;; By semantic constraints, classes that are defined at compile time
              ;; must still be defined at load time, and have the same superclasses
              ;; and metaclass. This would let us just serialize and check the CPL,
              ;; but to be a little flexible we only assume that it will still be a
              ;; class (i.e. so users can do technically-illegal redefinitions easier).
              `(subclassp (class-of ,object) (find-class ',head)))
             ;; Could be a literal class?
             ((and (null args) (clos::classp head))
              `(subclassp (class-of ,object) ,type))
             ;; We know nothing.
             (t
              ;; NOTE: In cclasp cleavir will wrap this with better source info.
              (cmp:warn-undefined-type nil type)
              (default)))))))))


(define-compiler-macro typep (&whole whole object type &optional environment
                                     &environment macro-env)
  (unless (and (constantp type macro-env) (null environment))
    (return-from typep whole))
  (let ((type (ext:constant-form-value type macro-env)))
    (typep-expansion object type macro-env whole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COERCE
;;;

(defun maybe-sequence-coercion-form (type env)
  (multiple-value-bind (kind length exactp success)
      (si::sequence-type-maker-info type env)
    (cond ((not success) nil)
          ((eq kind 'list)
           (let ((basic `(coerce-to-list object)))
             (if length ; type specified a length. fine.
                 `(let* ((coerced ,basic)
                         (lcoerced (length coerced)))
                    (unless (,(if exactp 'eql '>=) lcoerced ,length)
                      (core::error-sequence-length coerced ',type lcoerced))
                    coerced)
                 basic)))
          (t ; vector or ext-sequence.
           ;; We let make-sequence handle the length test.
           `(if (typep object ',type)
                object
                (replace (make-sequence ',type (length object)) object))))))

;;; FIXME: These should not exist. Instead, coerce should be inlined, and
;;; the compiler should eliminate tests where both arguments are constant.
;;; This will require expand-deftype or whatever to be constant foldable,
;;; which is a little weird as its behavior does change. Just, per
;;; the compiletime/runtime restrictions, any type defined at compile time
;;; has to be the same at runtime.
(defun expand-coercion (type env whole)
  (ext:with-current-source-form (type)
    (flet ((da (form) `(the (values ,type &rest nil) ,form)))
      (multiple-value-bind (head tail)
          (normalize-type type env)
        (case head
          ((t) (da 'object))
          ((character base-char) (da `(character object)))
          ;; make sure we don't convert other floats
          ((float) (da `(if (floatp object) object (float object))))
          ((short-float) (da `(float object 0.0s0)))
          ((single-float) (da `(float object 0.0f0)))
          ((double-float) (da `(float object 0.0d0)))
          ((long-float) (da `(float object 0.0l0)))
          ((function) (da `(coerce-to-function object)))
          ((complex)
           ;; This is the only case where the returned value
           ;; may not be of the provided type, due to complex rational rules.
           (destructuring-bind (&optional (realtype t) (imagtype t))
               tail
             `(complex (coerce (realpart object) ',realtype)
                       (coerce (imagpart object) ',imagtype))))
          ;; I don't think this is required or necessary, but we
          ;; already had it.
          ((and)
           (labels ((aux (form tail)
                      (if (= (length tail) 1)
                          `(coerce ,form ',(first tail))
                          (aux `(coerce ,form ',(first tail)) (rest tail)))))
             (if (= (length tail) 0)
                 `(the t object)
                 (aux 'object tail))))
          (t
           (let ((seqf (maybe-sequence-coercion-form type env)))
             (cond (seqf (da seqf))
                   (t
                    ;; Dunno what's going on. Punt to runtime.
                    ;; COERCE is actually defined for any type, provided
                    ;; that type exists: if the object is of the given
                    ;; type, it is returned, and otherwise a type-error
                    ;; is signaled. Nonetheless, if we reach here with a
                    ;; constant type, it's either undefined or does not
                    ;; have a coercion defined (e.g. INTEGER), which would
                    ;; be a weird thing to do. So we signal a style-warning.
                    ;; FIXME: We should differentiate "not defined" and
                    ;; "no coercion behavior", though.
                    ;; And maybe "can't figure it out at compile time but
                    ;; will at runtime".
                    (cmp:warn-cannot-coerce nil type)
                    whole)))))))))

(define-compiler-macro coerce (&whole form object type &environment env)
  (if (constantp type env)
      (let* ((type (ext:constant-form-value type env))
             (expansion (expand-coercion type env form)))
        (if (eq expansion form)
            form
            `(let ((object ,object)) ,expansion)))
      form))
