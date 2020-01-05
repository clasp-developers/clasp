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

(TEST
 types-classes-11-a
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'string (find-class 'string))
   (and subtype-p valid-p)))

(TEST
 types-classes-11-b
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'string) 'string)
   (and subtype-p valid-p)))

(TEST
 types-classes-12-a
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep  'BASE-STRING (find-class 'BASE-STRING))
   (and subtype-p valid-p)))

(TEST
 types-classes-12-b
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'BASE-STRING)  'BASE-STRING )
   (and subtype-p valid-p)))

(TEST
 types-classes-13-a
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep 'string (find-class 'string))
   (and subtype-p valid-p)))

(TEST
 types-classes-13-b
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep   (find-class 'SIMPLE-STRING) 'SIMPLE-STRING)
   (and subtype-p valid-p)))

(TEST
 types-classes-14-a
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep  'SIMPLE-BASE-STRING (find-class 'SIMPLE-BASE-STRING))
   (and subtype-p valid-p)))

(TEST
 types-classes-14-b
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep  (find-class 'SIMPLE-BASE-STRING)  'SIMPLE-BASE-STRING)
   (and subtype-p valid-p)))

(TEST
 types-classes-15-a
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep  'bit-vector (find-class 'bit-vector))
   (and subtype-p valid-p)))

(TEST
 types-classes-15-b
 (multiple-value-bind
       (subtype-p valid-p)
     (subtypep (find-class 'bit-vector)  'bit-vector)
   (and subtype-p valid-p)))
