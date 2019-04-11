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

(defun array-typep-form (form simplicity dims uaet)
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
      `(let ((object ,form))
         ,(case rank
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
             (generate-test simple-vector-type complex-vector-type
                            't 't
                            (generate-test simple-mdarray-type
                                           mdarray-type
                                           't 't 'nil)))
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
                           collect `(eq (core::%array-dimension object ',i)))))))))))

(defun cons-typep-form (form cart cdrt)
  `(let ((object ,form))
     (if (cleavir-primop:typeq object cons)
         (and ,@(if (eq cart '*)
                    nil
                    `((typep (cleavir-primop:car object) ',cart)))
              ,@(if (eq cdrt '*)
                    nil
                    `((typep (cleavir-primop:cdr object) ',cdrt))))
         nil)))

(defun number-typep-form (form head low high)
  ;; First, we special case FIXNUM and subintervals of fixnum.
  ;; Since the compiler macro normalizes, a literal 'fixnum type will
  ;; resolve as an integer interval,
  ;; but we don't want or need to do the compares.
  ;; NOTE: Even partially fixnum intervals could be optimized by
  ;; dividing into fixnum and bignum intervals - probably not
  ;; a priority though given how big fixnums can be
  (when (eq head 'integer)
    ;; We can turn exclusive ranges into inclusive ones.
    ;; (Even if it turns out to not be a fixnum- as long as it's an integer)
    (when (consp low) (setf low (1+ (car low))))
    (when (consp high) (setf high (1- (car high))))
    ;; OK, now check for fixnumitude.
    (when (and (not (eq low '*))
               (>= low most-negative-fixnum)
               (not (eq high '*))
               (<= high most-positive-fixnum))
      ;; It's a fixnum- this type test can therefore be very cheap,
      ;; doing only a header check and fixnum comparisons.
      ;; We can skip bounds checks if they happen to equal most-*-fixnum.
      ;; With that plus the strictness of the primops as conditions, this code
      ;; is written a little oddly, but generates something pretty obvious.
      ;; Remember: fixnum-not-greater is <=
      (let* ((highc (if (= high most-positive-fixnum)
                        't
                        `(if (cleavir-primop:fixnum-not-greater object ,high)
                             t nil)))
             (lowc (if (= low most-negative-fixnum)
                       highc
                       `(if (cleavir-primop:fixnum-not-greater ,low object)
                            ,highc nil))))
        (return-from number-typep-form
          `(let ((object ,form))
             (if (cleavir-primop:typeq object fixnum)
                 ,lowc
                 nil))))))
  ;; Non-fixnum, the general case.
  ;; TODO: Speed up float intervals by using primitive arithmetic.
  `(let ((object ,form))
     ;; bclasp typeq isn't smart enough to check the fixnum tag for types
     ;; like REAL, so we do that manually.
     (if ,(if (member head '(integer rational real))
              `(if (cleavir-primop:typeq object fixnum)
                   t
                   (if (cleavir-primop:typeq object ,head)
                       t nil))
              `(cleavir-primop:typeq object ,head))
         (and ,@(cond ((eq low '*) nil)
                      ((listp low) `((> object ',(car low))))
                      (t `((>= object ',low))))
              ,@(cond ((eq high '*) nil)
                      ((listp high) `((< object ',(car high))))
                      (t `((<= object ',high)))))
         nil)))

(define-compiler-macro typep (&whole whole object type &optional environment
                                     &environment macro-env)
  (unless (and (constantp type macro-env) (null environment))
    (return-from typep whole))
  (let ((type (ext:constant-form-value type macro-env)))
    (multiple-value-bind (head args) (normalize-type type)
      (case head
        ((t) `(progn ,object t))
        ((nil) `(progn ,object nil))
        ((and or)
         `(let ((object ,object))
            (,head ,@(mapcar
                      (lambda (type) `(typep object ',type))
                      args))))
        ((not)
         `(not (typep ,object ',(first args))))
        ((eql)
         `(eql ,object ',(first args)))
        ((satisfies)
         `(if (funcall (fdefinition ',(first args)) ,object) t nil))
        ((member)
         `(let ((object ,object))
            (or ,@(mapcar (lambda (x) `(eql object ',x)) args))))
        ((cons)
         (destructuring-bind (&optional (cart '*) (cdrt '*)) args
           (cons-typep-form object cart cdrt)))
        ((simple-array complex-array array)
         (destructuring-bind (&optional (et '*) (dims '*)) args
           (array-typep-form
            object head
            (if (integerp dims)
                (make-list dims :initial-element '*)
                dims)
            (if (eq et '*)
                et
                (upgraded-array-element-type et macro-env)))))
        ((sequence)
         `(let ((object ,object)) (or (listp object) (vectorp object))))
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
           ;; NOTE: We could warn on incorrectness.
           (unless (or (eq low '*)
                       ;; recursion should be ok since we check * first.
                       (typep low head)
                       (and (consp low) (null (cdr low))
                            (typep (car low) head)))
             (return-from typep whole))
           (unless (or (eq high '*)
                       (typep high head)
                       (and (consp high) (null (cdr high))
                            (typep (car high) head)))
             (return-from typep whole))
           (number-typep-form object head low high)))
        ((complex)
         ;; This covers (complex whatever) types in addition to just complex.
         ;; We don't have multiple complex types in the backend,
         ;; so we can just do this.
         `(complexp ,object))
        (otherwise
         ;; Last ditch efforts.
         (cond
           ;; Is there a simple predicate?
           ((simple-type-predicate type)
            `(,(simple-type-predicate type) ,object))
           ;; Could be a C++ class kind of thing.
           ;; NOTE: This is a static header check, so it shouldn't be used
           ;; for anything that could be subclassed. The most likely candidate
           ;; for this problem is STREAM, but it's caught by the previous case.
           ((gethash type core:+type-header-value-map+)
            `(if (cleavir-primop:typeq ,object ,type) t nil))
           ;; Maybe it's a class name?
           ((and (symbolp type) (find-class type nil macro-env))
            ;; By semantic constraints, classes that are defined at compile time
            ;; must still be defined at load time, and have the same superclasses
            ;; and metaclass. This would let us just serialize and check the CPL,
            ;; but to be a little flexible we only assume that it will still be a
            ;; class (i.e. so users can do technically-illegal redefinitions easier).
            `(subclassp (class-of ,object) (find-class ',type)))
           ;; Could be a literal class?
           ((clos::classp type)
            `(subclassp (class-of ,object) ,type))
           ;; We know nothing.
           (t
            whole)))))))

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
                         (if validp
                             (if (eq uaet 'list)
                                 (da `(coerce-to-list ,obj))
                                 (da `(make-array (length ,obj)
                                                  :element-type ',uaet
                                                  :initial-contents ,obj)))
                             ;; Dunno what's going on. Punt to runtime.
                             (return-from coerce form))))))))))
      form))
