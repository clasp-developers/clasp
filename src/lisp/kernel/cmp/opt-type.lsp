(in-package #:core)

(core:bclasp-define-compiler-macro typep (&whole whole object type &optional environment
                                     &environment macro-env)
  (unless (constantp type macro-env)
    (return-from typep whole))
  (let ((type (ext:constant-form-value type macro-env)))
    (cond (environment whole)
          ((eq type t)
           `(progn
              ,object
              t))
          ((null type)
           `(progn
              ,object
              nil))
          ((eq type 'sequence)
           `(let ((object ,object))
              (or (listp object) (vectorp object))))
          ((eq type 'simple-base-string)
           `(let ((object ,object))
              (and (base-string-p object)
                   (simple-string-p object))))
          ((and (symbolp type)
                (let ((predicate (simple-type-predicate type)))
                  (and predicate `(,predicate ,object)))))
          ((and (proper-list-p type)
                (eq (car type) 'member))
           `(let ((object ,object))
              (or ,@(mapcar
                     (lambda (x)
                       `(eq object ',x))
                     (cdr type)))))
          ((and (consp type)
                (eq (car type) 'eql)
                (cdr type)
                (not (cddr type)))
           `(eql ,object ',(cadr type)))
          ((and (symbolp type)
                (find-class type nil))
           ;; Finding a class at compile time and not load time is disallowed by semantic constraints
           `(subclassp (class-of ,object) (find-class ',type)))
          (t
           whole))))

;;; FIXME: This should not exist. Instead, coerce should be inlined, and
;;; the compiler should eliminate tests where both arguments are constant.
;;; This will require expand-deftype or whatever to be constant foldable,
;;; which is a little weird as its behavior does change. Just, per
;;; the compiletime/runtime restrictions, any type defined at compile time
;;; has to be the same at runtime.
(core:bclasp-define-compiler-macro coerce (&whole form object type &environment env)
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
