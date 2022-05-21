(in-package #:clasp-tests)

(defmacro test-subtypep (name
                         type-sub type-super
                         expected-subtype-p
                         expected-valid-p)
  `(test ,name (subtypep ,type-sub ,type-super)
         (,expected-subtype-p ,expected-valid-p)))

(defmacro test-types-classes (lname rname type)
  `(progn
     (test-true ,lname
                (multiple-value-bind (st vp)
                    (subtypep ',type (find-class ',type))
                  (and st vp)))
     (test-true ,rname
                (multiple-value-bind (st vp)
                    (subtypep (find-class ',type) ',type)
                  (and st vp)))))

(test-types-classes types-classes-1 types-classes-2 fixnum)
(test-types-classes types-classes-3 types-classes-4 bignum)
(test-types-classes types-classes-5 types-classes-6 long-float)
(test-types-classes types-classes-7 types-classes-8 short-float)

(test-subtypep types-classes-9
               (type-of #2a((nil nil) (nil nil)))
               (class-of #2a((nil nil) (nil nil)))
               t t)

(test-subtypep types-classes-10
               (type-of #'car) 'function t t)

(test-true ARRAY.9.8
           (let () (TYPEP #2A((A B) (C D) (E F)) '(SIMPLE-ARRAY * (* 2)))))

(test-types-classes types-classes-11-a types-classes-11-b string)
(test-types-classes types-classes-12-a types-classes-12-b base-string)
(test-types-classes types-classes-13-a types-classes-13-b simple-string)
(test-types-classes types-classes-14-a types-classes-14-b simple-base-string)
(test-types-classes types-classes-15-a types-classes-15-b bit-vector)

(test-subtypep subtypep-bug-979
               't '(cons (and standard-char (member #\@)) real)
               nil nil)


(defstruct %semaphore
  lock
  condition-variable
  counter)

(deftype semaphore ()
  '%semaphore)

(defun semaphore-p (object)
  "Returns T if OBJECT is a semaphore; returns NIL otherwise."
  (typep object 'semaphore))

(test-true issue-1252
           (semaphore-p (make-%semaphore)))

(deftype gesture-name ()
  'symbol)

(test-true issue-1252a
           (let ((name :abort))
             (typep name 'gesture-name)))

(test-true issue-1308
           (let ()
             (null (typep #() '(array * nil)))))
