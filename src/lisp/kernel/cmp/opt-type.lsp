(in-package #:core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEP
;;;

;;; TODO: Treat malformed types better:
;;; We should DEFINITELY NOT cause compile time errors
;;; We COULD signal warnings on types like (standard-char foo)

(defun cons-typep-form (form cart cdrt)
  `(let ((object ,form))
     (and (consp object)
          ,@(if (eq cart '*)
                nil
                `((typep (car object) ',cart)))
          ,@(if (eq cdrt '*)
                nil
                `((typep (cdr object) ',cdrt))))))

(defun array-typep-form (form et dims base-predicate env)
  `(let ((object ,form))
     (and
      ;; Basic: Is it an array at all.
      (,base-predicate object)
      ;; Does the element type match?
      ,@(if (eq et '*)
            nil
            (let ((uaet (upgraded-array-element-type et env)))
              ;; I use EQ since UAETs are always symbols at the moment.
              `((eq ',uaet (array-element-type object)))))
      ;; Do the dimensions match?
      ,@(if (eq dims '*)
            nil
            (let ((dims (if (integerp dims)
                            (make-list dims :initial-element '*)
                            dims)))
              ;; all numbers involved are fixnums, so EQ is ok.
              `((eq (array-rank object) ',(length dims))
                ,@(loop for dim in dims
                        for i from 0
                        unless (eq dim '*)
                          collect `(eq (array-dimension object ',i)
                                       ',dim))))))))

(defun number-typep-form (form simple-pred low high)
  `(let ((object ,form))
     (and (,simple-pred object)
          ;; We ought to make sure that low and high are the right kind of
          ;; number and stuff like that. Maybe.
          ,@(cond ((eq low '*) nil)
                  ((listp low) `((> object ',(car low))))
                  (t `((>= object ',low))))
          ,@(cond ((eq high '*) nil)
                  ((listp high) `((< object ',(car high))))
                  (t `((<= object ',high)))))))

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
         (let ((pred (simple-type-predicate head)))
           (when (null pred)
             (error "BUG: Missing simple type predicate for ~a" head))
           (destructuring-bind (&optional (et '*) (dims '*)) args
             (array-typep-form object et dims pred macro-env))))
        ((sequence)
         `(let ((object ,object)) (or (listp object) (vectorp object))))
        ((standard-char)
         `(let ((object ,object))
            (and (characterp object) (standard-char-p object))))
        ((bignum)
         `(let ((object ,object))
            (and (integerp object) (not (fixnump object)))))
        ((#+short-float short-float #+long-float long-float
          single-float double-float
          float integer rational real)
         (let ((pred (simple-type-predicate head)))
           (when (null pred)
             (error "BUG: Missing simple type predicate for ~a" head))
           (destructuring-bind (&optional (low '*) (high '*)) args
             (number-typep-form object pred low high))))
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
