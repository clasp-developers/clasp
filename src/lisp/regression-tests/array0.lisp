(in-package #:clasp-tests)

(test array-dimension0 (= (array-dimension (make-array '(3 4) :element-type t) 0) 3))
;;(test (subtypep (class-of "abc") 'string))
(test make-sequence-simple-string
      (subtypep (type-of (make-sequence 'simple-string 10)) '(simple-array character (10))))
(test make-sequence-simple-base-string
      (subtypep (type-of (make-sequence 'simple-base-string 10)) '(simple-array base-char (10))))
(test typep-make-simple-array (typep (make-array '(10 10)) 'simple-array))
(test typep-make-fill-pointer-vector-nonsimple (not (typep (make-array 10 :fill-pointer 3) 'simple-array)))
(test typep-make-fill-pointer-vector-nonsimple-vector (not (typep (make-array 10 :fill-pointer 3) 'simple-vector)))
(test typep-make-adjustable-array (typep (make-array '(10 10) :adjustable t) 'array))
(test typep-make-adjustable-array-nonsimple (not (typep (make-array '(10 10) :adjustable t) 'simple-array)))
(test typep-make-adjustable-array-non-simple-vector (not (typep (make-array '(10 10) :adjustable t) 'simple-vector)))
(test adjust-array0 (equalp (adjust-array #2A((1 2) (3 4)) '(3 3)) #2A((1 2 nil) (3 4 nil) (nil nil nil))))
(test make-array-0 (null (array-displacement (make-array 5 :element-type 'CHARACTER :initial-element #\a :adjustable t))))
(test make-array-1 (null (array-displacement (make-array 5 :element-type 'BASE-CHAR :initial-element #\a :adjustable t))))
(test make-array-2 (arrayp (handler-case
                               (make-array '(2 2) :element-type '(integer 0 (256))
                                           :initial-contents '((34 98)(14 119)))
                             (error (e) e))))
(test make-array-3 (let ((array (handler-case
                                    (list (make-array '(0) :element-type nil))
                                  (error () nil))))
                     (or (null array)(arrayp array))))

(test make-array-4 (let ((please-inline (MAKE-ARRAY '(2 3) :INITIAL-ELEMENT #\a :ELEMENT-TYPE 'character)))
                     (char= #\a (aref please-inline 0 0))))

(test make-array-5 (let ((please-inline (MAKE-ARRAY '(2 3) :INITIAL-ELEMENT #\a :ELEMENT-TYPE 'base-char)))
                     (char= #\a (aref please-inline 0 0))))

(test-expect-error make-array-6
                   (make-array 5 :element-type (array-element-type "") :displaced-index-offset 2 :displaced-to "")
                   :type simple-error)

(test aref-nil-array
      (let ((array (make-array nil)))
        (aref array)
        array))

(test setf-aref-nil-array
      (let ((array (make-array nil)))
        (setf (aref array) 23)
        array))

;;; gave segmentation violation printing the error
(test-expect-error fill-pointer-nil-array (fill-pointer #0anil) :type type-error)

;;; used to give off by 1 wrong error message about the upper bound of the limit
;;; Right message is Invalid index 5 for axis 0 of array: expected 0-2
(test
 simple-array-out-of-bounds-message
 (search "expected 0-2"
         (let ()
           (handler-case 
               (locally (declare (optimize (speed 0)(safety 3)))
                 (aref (make-array 3 :initial-element 5) 5)
                 "nada")
             (error (e)
               (princ-to-string e))))))
                   
