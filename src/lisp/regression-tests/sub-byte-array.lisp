(in-package #:clasp-tests)

;;;; Test reading from and writing to sub-byte arrays, because they're more
;;;; complicated to access (masking etc.)

;;; the intent here is to make sure the array cross multiple words, and also ends with
;;; a partial word to make sure the bit ordering is ok.
(defconstant +irregular-suffix-length+ 37)
(defconstant +test-array-length+ (+ 64 37))

(declaim (notinline unoptimized-vref (setf unoptimized-vref)))
(defun unoptimized-vref (array index)
  (declare (optimize debug (speed 0)))
  (aref array index))
(defun (setf unoptimized-vref) (new array index)
  (declare (optimize debug (speed 0)))
  (setf (aref array index) new))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun test-array-elements (bits signedp)
  (let* ((max (ash 1 bits))
         (sub (if signedp (ash 1 (1- bits)) 0))
         (forward (loop for i below 32
                        for e = (- (mod i max) sub)
                        collect e))
         (reverse (loop for i below 32
                        for e = (- (- max (mod i max) 1) sub)
                        collect e))
         (irregular (loop for i below +irregular-suffix-length+
                          collect (- (mod i max) sub))))
    (append forward reverse irregular)))
) ; eval-when

(macrolet ((unopt->opt-test (name element-type bits signedp)
             (let ((elements (test-array-elements bits signedp)))
               `(test ,name
                      (let ((array (make-array +test-array-length+
                                               :element-type ',element-type
                                               :initial-contents ',elements)))
                        ;; should be inferred, but just in case
                        (declare (type (simple-array ,element-type (,+test-array-length+)) array))
                        (loop for i below +test-array-length+
                              collect (aref array i)))
                      (,elements)))))
  (unopt->opt-test array-byte2-opt-read (unsigned-byte 2) 2 nil)
  (unopt->opt-test array-byte4-opt-read (unsigned-byte 4) 4 nil)
  (unopt->opt-test array-int2-opt-read (signed-byte 2) 2 t)
  (unopt->opt-test array-int4-opt-read (signed-byte 4) 4 t))

(macrolet ((opt->unopt-test (name element-type bits signedp)
             (let ((elements (test-array-elements bits signedp)))
               `(test ,name
                      (let ((array (make-array +test-array-length+
                                               :element-type ',element-type)))
                        ;; should be inferred, but just in case
                        (declare (type (simple-array ,element-type (,+test-array-length+)) array))
                        (loop for i below +test-array-length+
                              do (setf (aref array i) (elt ',elements i)))
                        (loop for i below +test-array-length+
                              collect (unoptimized-vref array i)))
                      (,elements)))))
  (opt->unopt-test array-byte2-opt-write (unsigned-byte 2) 2 nil)
  (opt->unopt-test array-byte4-opt-write (unsigned-byte 4) 4 nil)
  (opt->unopt-test array-int2-opt-write (signed-byte 2) 2 t)
  (opt->unopt-test array-int4-opt-write (signed-byte 4) 4 t))

(macrolet ((opt->opt-test (name element-type bits signedp)
             (let ((elements (test-array-elements bits signedp)))
               `(test ,name
                      (let ((array (make-array +test-array-length+
                                               :element-type ',element-type)))
                        ;; should be inferred, but just in case
                        (declare (type (simple-array ,element-type (,+test-array-length+)) array))
                        (loop for i below +test-array-length+
                              do (setf (aref array i) (elt ',elements i)))
                        (loop for i below +test-array-length+
                              collect (aref array i)))
                      (,elements)))))
  (opt->opt-test array-byte2-opt-readwrite (unsigned-byte 2) 2 nil)
  (opt->opt-test array-byte4-opt-readwrite (unsigned-byte 4) 4 nil)
  (opt->opt-test array-int2-opt-readwrite (signed-byte 2) 2 t)
  (opt->opt-test array-int4-opt-readwrite (signed-byte 4) 4 t))
