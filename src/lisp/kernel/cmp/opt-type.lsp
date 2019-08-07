(in-package #:core)

;;;; Optimizations for Chapter 4, Types and Classes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEP
;;;

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
                          '(bit fixnum ext:byte8 ext:byte16 ext:byte32 ext:byte64
                            ext:integer8 ext:integer16 ext:integer32 ext:integer64
                            single-float double-float base-char character t)
                          for uaet in types
                          collect (cons element-type uaet)))
                (defun ,name (uaet)
                  (if (eq uaet '*)
                      ',untyped
                      (let ((pair (assoc uaet ,map-name)))
                        (if pair
                            (cdr pair)
                            (error "BUT: Unknown UAET ~a in ~a" uaet ',name))))))))
  (def simple-vector-type +simple-vector-type-map+ core:abstract-simple-vector
    simple-bit-vector
    core:simple-vector-fixnum
    core:simple-vector-byte8-t
    core:simple-vector-byte16-t
    core:simple-vector-byte32-t
    core:simple-vector-byte64-t
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
    core:complex-vector-byte8-t
    core:complex-vector-byte16-t
    core:complex-vector-byte32-t
    core:complex-vector-byte64-t
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
    core:simple-mdarray-byte8-t
    core:simple-mdarray-byte16-t
    core:simple-mdarray-byte32-t
    core:simple-mdarray-byte64-t
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
    core:mdarray-byte8-t
    core:mdarray-byte16-t
    core:mdarray-byte32-t
    core:mdarray-byte64-t
    core:mdarray-int8-t
    core:mdarray-int16-t
    core:mdarray-int32-t
    core:mdarray-int64-t
    core:mdarray-float
    core:mdarray-double
    core:mdarray-base-char
    core:mdarray-character
    core:mdarray-t))

(defun array-typep-form (simplicity dims uaet)
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
                `(if (cleavir-primop:typeq object ,complex)
                     ,if-complex ,if-no))
               ((array)
                `(if (cleavir-primop:typeq object ,simple)
                     ,if-simple
                     (if (cleavir-primop:typeq object ,complex)
                         ,if-complex ,if-no))))))
      (case rank
        ((1) ; vector
         (let ((length (first dims)))
           (if (eq length '*)
               ;; no length check: easy
               (generate-test simple-vector-type complex-vector-type
                              't 't 'nil)
               ;; Now we have to get the length differently based on
               ;; the simplicity.
               (generate-test simple-vector-type complex-vector-type
                              `(eq (core::vector-length object) ',length)
                              `(eq (core::%array-dimension object 0) ',length)
                              'nil))))
        ((*) ; anything, and dimensions are unspecified
         ;; for general arrays we have superclasses to use
         (if (and (eq uaet '*) (eq simplicity 'array))
             `(if (cleavir-primop:typeq object array) t nil)
             (generate-test simple-vector-type complex-vector-type
                            't 't
                            (generate-test simple-mdarray-type
                                           mdarray-type
                                           't 't 'nil))))
        (otherwise ; an mdarray with possibly specified dimensions
         `(block nil
            ;; We use a block so the dimensions check code is only
            ;; generated once.
            ;; First, if it's not an mdarray, return NIL early.
            ,(generate-test simple-mdarray-type mdarray-type
                            nil nil '(return nil))
            ;; Now, it is an mdarray, so check dimensions.
            (and
             ,@(loop for dim in dims
                     for i from 0
                     unless (eq dim '*)
                       collect `(eq (core::%array-dimension object ',i) ,dim)))))))))

(defun cons-typep-form (cart cdrt env)
  `(if (cleavir-primop:typeq object cons)
       (and ,@(if (eq cart '*)
                  nil
                  (list
                   `(let ((object (cleavir-primop:car object)))
                      ,(typep-expansion cart env))))
            ,@(if (eq cdrt '*)
                  nil
                  (list
                   `(let ((object (cleavir-primop:cdr object)))
                      ,(typep-expansion cdrt env)))))
       nil))

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

;;; This is more complicated than for the other real types because of
;;; the fixnum/bignum split.
(defun integral-interval-typep-form (low high)
  ;; Turn exclusive ranges into inclusive ones.
  (when (consp low) (setf low (1+ (car low))))
  (when (consp high) (setf high (1- (car high))))
  ;; Now we basically want to break things up into three ranges:
  ;; fixnum, negative bignum, and positive bignum.
  ;; at most two of these may be empty.
  ;; FIXME: Check for stupid empty types like (integer (0) 0) or (integer 3 -3)
  (multiple-value-bind (bignum-low fixnum-low)
      (cond
        ;; All negative bignums are included, and no limit for fixnums
        ((eq low '*) (values low low))
        ;; Some negative bignums are included, and no limit for fixnums
        ((< low most-negative-fixnum) (values low '*))
        ;; No negative bignums are included, but all negative fixnums are
        ((= low most-negative-fixnum) (values nil '*))
        ;; No negative bignums, some fixnums
        ((<= low most-positive-fixnum) (values nil low))
        ;; No negative bignums and no fixnums
        (t (values nil nil)))
    (multiple-value-bind (bignum-high fixnum-high)
        (cond
          ((eq high '*) (values high high))
          ((> high most-positive-fixnum) (values high '*))
          ((= high most-positive-fixnum) (values nil '*))
          ((>= high most-negative-fixnum) (values nil high))
          (t (values nil nil)))
      ;; Now the actual tests
      (let ((bignum-test
              (if (and (null bignum-low) (null bignum-high)) ; none
                  'nil
                  `(if (cleavir-primop:typeq object bignum)
                       (and ,@(cond ((null bignum-low) ; no negative bignums
                                     `((core:two-arg-> object ,most-positive-fixnum)))
                                    ((eq bignum-low '*) ; all negative bignums
                                     nil)
                                    (t ; only some
                                     `((core:two-arg->= object ,bignum-low))))
                            ,@(cond ((null bignum-high) ; no positive bignums
                                     `((core:two-arg-< object ,most-negative-fixnum)))
                                    ((eq bignum-high '*) ; all positive bignums
                                     nil)
                                    (t ; only some
                                     `((core:two-arg-<= object ,bignum-high)))))
                       'nil))))
        ;; fixnums
        (if (or (null fixnum-low) (null fixnum-high)) ; none
            bignum-test
            (let* ((high-test
                     (if (eq fixnum-high '*)
                         't
                         `(if (cleavir-primop:fixnum-not-greater object ,fixnum-high) t nil)))
                   (low-test
                     (if (eq fixnum-low '*)
                         high-test
                         `(if (cleavir-primop:fixnum-not-greater ,fixnum-low object)
                              ,high-test
                              nil))))
              `(if (cleavir-primop:typeq object fixnum) ,low-test ,bignum-test)))))))

;;; The simpler version
;;; FIXME: Use floating point compares, etc, when available
(defun real-interval-test (low high)
  `(and ,@(cond ((eq high '*) nil)
                ((consp high) `((< object ,(car high))))
                (t `((<= object ,high))))
        ,@(cond ((eq low '*) nil)
                ((consp low) `((> object ,(car low))))
                (t `((>= object ,low))))))

(defun real-interval-typep-form (head low high)
  (ecase head
    ((integer) (integral-interval-typep-form low high))
    ((rational)
     `(or ,(integral-interval-typep-form low high)
          (if (cleavir-primop:typeq object ratio)
              ,(real-interval-test low high)
              nil)))
    ((short-float single-float double-float long-float)
     `(if (cleavir-primop:typeq object ,head)
          ,(real-interval-test low high)
          nil))
    ((float)
     ;; only singles and doubles actually exist.
     ;; FIXME: write in this assumption better in case we change it later.
     `(if (if (cleavir-primop:typeq object single-float)
              t
              (if (cleavir-primop:typeq object double-float) t nil))
          ,(real-interval-test low high)
          nil))
    ((real)
     `(or ,(integral-interval-typep-form low high)
          (if (if (cleavir-primop:typeq object single-float)
                  t
                  (if (cleavir-primop:typeq object double-float)
                      t
                      (if (cleavir-primop:typeq object ratio) t nil)))
              ,(real-interval-test low high)
              nil)))))

(defun typep-expansion (type env &optional (form nil formp))
  (flet ((default () (if formp form `(typep object ',type))))
    (multiple-value-bind (head args) (normalize-type type)
      (case head
        ((t) 't)
        ((nil) 'nil)
        ((and or)
         `(,head ,@(mapcar (lambda (type) (typep-expansion type env)) args)))
        ((not) `(not ,(typep-expansion (first args) env)))
        ((eql) `(eql object ',(first args)))
        ((satisfies)
         `(if (funcall (fdefinition ',(first args)) object) t nil))
        ((member)
         `(or ,@(mapcar (lambda (x) `(eql object ',x)) args)))
        ((cons)
         (destructuring-bind (&optional (cart '*) (cdrt '*)) args
           (cons-typep-form cart cdrt env)))
        ((simple-array complex-array array)
         (destructuring-bind (&optional (et '*) (dims '*)) args
           (array-typep-form
            head
            (if (integerp dims)
                (make-list dims :initial-element '*)
                dims)
            (if (eq et '*)
                et
                (upgraded-array-element-type et env)))))
        ((sequence)
         `(or (listp object) (vectorp object)))
        ((standard-char)
         `(and (characterp object) (standard-char-p object)))
        ;; NOTE: Probably won't actually occur, due to normalization.
        ((bignum)
         `(and (integerp object) (not (fixnump object))))
        ((#+short-float short-float #+long-float long-float
          single-float double-float
          float integer rational real)
         (destructuring-bind (&optional (low '*) (high '*)) args
           ;; Check the bounds for correctness.
           ;; We use primitives for testing with them, so we want
           ;; to be sure that they're valid.
           (cond ((valid-number-type-p head low high)
                  (real-interval-typep-form head low high))
                 (t (cmp:warn-invalid-number-type nil type)
                    (default)))))
        ((complex)
         ;; This covers (complex whatever) types in addition to just complex.
         ;; We don't have multiple complex types in the backend,
         ;; so we can just do this.
         ;; See comment in DEFUN TYPEP in lsp/predlib.lsp.
         `(complexp object))
        (otherwise
         ;; Last ditch efforts.
         (cond
           ;; Is there a simple predicate?
           ((simple-type-predicate type)
            `(,(simple-type-predicate type) object))
           ;; Could be a C++ class kind of thing.
           ;; NOTE: This is a static header check, so it shouldn't be used
           ;; for anything that could be subclassed. The most likely candidate
           ;; for this problem is STREAM, but it's caught by the previous case.
           ((gethash type core:+type-header-value-map+)
            `(if (cleavir-primop:typeq object ,type) t nil))
           ;; Maybe it's a class name? (See also, comment in clos/defclass.lsp.)
           ((and (symbolp type) (class-info type env))
            ;; By semantic constraints, classes that are defined at compile time
            ;; must still be defined at load time, and have the same superclasses
            ;; and metaclass. This would let us just serialize and check the CPL,
            ;; but to be a little flexible we only assume that it will still be a
            ;; class (i.e. so users can do technically-illegal redefinitions easier).
            `(subclassp (class-of object) (find-class ',type)))
           ;; Could be a literal class?
           ((clos::classp type)
            `(subclassp (class-of object) ,type))
           ;; We know nothing.
           (t
            ;; NOTE: In cclasp cleavir will wrap this with better source info.
            (cmp:warn-undefined-type nil type)
            (default))))))))

(define-compiler-macro typep (&whole whole object type &optional environment
                                     &environment macro-env)
  (unless (and (constantp type macro-env) (null environment))
    (return-from typep whole))
  (let* ((type (ext:constant-form-value type macro-env))
         (expanded (typep-expansion type macro-env whole)))
    (if (eq expanded whole)
        whole ; failure
        `(let ((object ,object)) ,expanded))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COERCE
;;;

;;; FIXME: This should not exist. Instead, coerce should be inlined, and
;;; the compiler should eliminate tests where both arguments are constant.
;;; This will require expand-deftype or whatever to be constant foldable,
;;; which is a little weird as its behavior does change. Just, per
;;; the compiletime/runtime restrictions, any type defined at compile time
;;; has to be the same at runtime.
(define-compiler-macro coerce (&whole form object type &environment env)
  (if (constantp type env)
      (let ((type (ext:constant-form-value type env))
            (obj (gensym "OBJECT")))
        `(let ((,obj ,object))
           ;; this check is required by the definition of coerce.
           ;; of course, we should eliminate it later.
           (if (typep ,obj ',type)
               ,obj
               ,(flet ((da (form) `(the (values ,type &rest nil) ,form)))
                  (multiple-value-bind (head tail)
                      (normalize-type type)
                    (case head
                      ((t) (da obj))
                      ((character base-char) (da `(character ,obj)))
                      ((float) (da `(float ,obj)))
                      ((short-float) (da `(float ,obj 0.0s0)))
                      ((single-float) (da `(float ,obj 0.0f0)))
                      ((double-float) (da `(float ,obj 0.0d0)))
                      ((long-float) (da `(float ,obj 0.0l0)))
                      ((function) (da `(coerce-to-function ,obj)))
                      ((complex)
                       ;; This is the only case where the returned value
                       ;; may not be of the provided type, due to complex rational rules.
                       (destructuring-bind (&optional (realtype t) (imagtype t))
                           tail
                         `(complex (coerce (realpart ,obj) ',realtype)
                                   (coerce (imagpart ,obj) ',imagtype))))
                      ;; I don't think this is required or necessary, but we
                      ;; already had it.
                      ((and)
                       (labels ((aux (form tail)
                                  (if (= (length tail) 1)
                                      `(coerce ,form ,(first tail))
                                      (aux `(coerce ,form ,(first tail)) (rest tail)))))
                         (if (= (length tail) 0)
                             `(the t ,obj)
                             (aux obj tail))))
                      (t ; a sequence type, we figure
                       (multiple-value-bind (uaet length validp)
                           (closest-sequence-type type env)
                         (cond
                           (validp
                            (if (eq uaet 'list)
                                (da `(coerce-to-list ,obj))
                                (da `(make-array (length ,obj)
                                                 :element-type ',uaet
                                                 :initial-contents ,obj))))
                           (t ; Dunno what's going on. Punt to runtime.
                            ;; COERCE is actually defined for any type, provided
                            ;; that type exists: if the object is of the given
                            ;; type, it is returned, and otherwise a type-error
                            ;; is signaled. Nonetheless, if we reach here with a
                            ;; constant type, it's either undefined or does not
                            ;; have a coercion defined (e.g. INTEGER), which would
                            ;; be a weird thing to do. So we signal a style-warning.
                            ;; FIXME: We should differentiate "not defined" and
                            ;; "no coercion behavior", though.
                            (cmp:warn-cannot-coerce nil type)
                            (return-from coerce form)))))))))))
      form))
