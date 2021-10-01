(in-package #:clasp-tests)

(test array-dimension0 (array-dimension (make-array '(3 4) :element-type t) 0)
      (3))
;;(test (subtypep (class-of "abc") 'string))
(test-true make-sequence-simple-string
           (subtypep (type-of (make-sequence 'simple-string 10))
                     '(simple-array character (10))))
(test-true make-sequence-simple-base-string
           (subtypep (type-of (make-sequence 'simple-base-string 10))
                     '(simple-array base-char (10))))
(test-type typep-make-simple-array (make-array '(10 10)) simple-array)
(test-type typep-make-fill-pointer-vector-nonsimple
    (make-array 10 :fill-pointer 3) (not simple-array))
(test-type typep-make-fill-pointer-vector-nonsimple-vector
    (make-array 10 :fill-pointer 3) (not simple-vector))
(test-type typep-make-adjustable-array
    (make-array '(10 10) :adjustable t) array)
(test-type typep-make-adjustable-array-nonsimple
    (make-array '(10 10) :adjustable t) (not simple-array))
(test-type typep-make-adjustable-array-non-simple-vector
    (make-array '(10 10) :adjustable t) (not simple-vector))
(test adjust-array0
      (adjust-array #2A((1 2) (3 4)) '(3 3))
      (#2A((1 2 nil) (3 4 nil) (nil nil nil))))
(test make-array-0
      (array-displacement
       (make-array 5 :element-type 'CHARACTER :initial-element #\a :adjustable t))
      (nil 0))
(test make-array-1
      (array-displacement
       (make-array 5 :element-type 'BASE-CHAR :initial-element #\a :adjustable t))
      (nil 0))
(test-type make-array-2
    (make-array '(2 2) :element-type '(integer 0 (256))
                       :initial-contents '((34 98)(14 119)))
    array)
(test-true make-array-3
           (let ((array (handler-case
                            (list (make-array '(0) :element-type nil))
                          (error () nil))))
             (or (null array) (arrayp array))))

(test make-array-4
      (let ((please-inline
              (MAKE-ARRAY '(2 3) :INITIAL-ELEMENT #\a :ELEMENT-TYPE 'character)))
        (aref please-inline 0 0))
      (#\a))

(test make-array-5
      (let ((please-inline
              (MAKE-ARRAY '(2 3) :INITIAL-ELEMENT #\a :ELEMENT-TYPE 'base-char)))
        (aref please-inline 0 0))
      (#\a))

(test-expect-error make-array-6
                   (make-array 5 :element-type (array-element-type "") :displaced-index-offset 2 :displaced-to "")
                   :type simple-error)

(test aref-nil-array
      (let ((array (make-array nil :initial-element 23)))
        (aref array))
      (23))

(test setf-aref-nil-array
      (let ((array (make-array nil)))
        (setf (aref array) 23)
        array)
      (#0a23))

;;; gave segmentation violation printing the error
(test-expect-error fill-pointer-nil-array (fill-pointer #0anil) :type type-error)

;;; used to give off by 1 wrong error message about the upper bound of the limit
;;; Right message is Invalid index 5 for axis 0 of array: expected 0-2 or
;;; Row-major array index 5 is out of bounds (INTEGER 0 (3)).
(test-true
 simple-array-out-of-bounds-message
 (let ((message
         (handler-case 
             (locally (declare (optimize (speed 0)(safety 3)))
               (aref (make-array 3 :initial-element 5) 5)
               "nada")
           (error (e)
             (princ-to-string e)))))
   (or  (search "expected 0-2" message)
        (search "(INTEGER 0 (3))" message))))

;;; Drmeister on #clasp
;;; Bike: (make-array '(nil 5)) doesn't generate an error but it should

(test-expect-error array-too-big
                   (make-array (1+ array-total-size-limit))
                   :type type-error)

(test-expect-error array-too-big-list
                   (make-array (list (1+ array-total-size-limit)))
                   :type type-error)

(test-expect-error array-wrong-dimension-list-a
                   (make-array (list nil 13))
                   :type type-error)

(test-expect-error array-wrong-dimension-list-b
                   (make-array (list #\a 13))
                   :type type-error)

(test-expect-error array-wrong-dimension-list-c
                   (make-array (list -13))
                   :type type-error)

;;; and w/o compiler-macro
(test-expect-error array-too-big-not-inline
                   (locally (declare (notinline make-array))
                     (make-array (1+ array-total-size-limit)))
                   :type type-error)

(test-expect-error array-too-big-list-not-inline
                    (locally (declare (notinline make-array))
                      (make-array (list (1+ array-total-size-limit))))
                   :type type-error)

(test-expect-error array-wrong-dimension-list-a-not-inline
                   (locally (declare (notinline make-array))
                     (make-array (list nil 13)))                 
                   :type type-error)

(test-expect-error array-wrong-dimension-list-b-not-inline
                    (locally (declare (notinline make-array))
                      (make-array (list #\a 13)))
                   :type type-error)

(test-expect-error array-wrong-dimension-list-c-not-inline
                    (locally (declare (notinline make-array))
                      (make-array (list -13)))
                   :type type-error)
