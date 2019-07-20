(in-package #:clasp-tests)

(TEST
 types-classes-1
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'fixnum (find-class 'fixnum))
   (and subtype-p valid-p)))

(TEST
 types-classes-2
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'fixnum) 'fixnum)
   (and subtype-p valid-p)))

(TEST
 types-classes-3
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'bignum (find-class 'bignum))
   (and subtype-p valid-p)))

(TEST
 types-classes-4
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'bignum) 'bignum)
   (and subtype-p valid-p)))

(TEST
 types-classes-5
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'long-float (find-class 'long-float))
   (and subtype-p valid-p)))

(TEST
 types-classes-6
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'long-float) 'long-float)
   (and subtype-p valid-p)))

(TEST
 types-classes-7
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'short-float (find-class 'short-float))
   (and subtype-p valid-p)))

(TEST
 types-classes-8
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'short-float) 'short-float)
   (and subtype-p valid-p)))

(TEST
 types-classes-9
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (type-of #2A((NIL NIL) (NIL NIL)))(class-of #2A((NIL NIL) (NIL NIL))))
   (and subtype-p valid-p)))

(TEST
 types-classes-10
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (type-of #'car) 'function)
   (and subtype-p valid-p)))

(test ARRAY.9.8
 (let () (TYPEP #2A((A B) (C D) (E F)) '(SIMPLE-ARRAY * (* 2)))))
