(in-package #:clasp-tests)

;;; First easiest bitvectors
(test standard-bitvector-1
      (let ((foo-1 (make-array 2 :element-type 'bit :initial-element 0))
            (foo-2 (make-array 2 :element-type 'bit :initial-element 1)))
        (bit-and foo-1 foo-2))
      (#*00))

(test standard-bitvector-2
      (let ((foo-3 (make-array 2 :element-type 'bit :initial-element 1))
            (foo-2 (make-array 2 :element-type 'bit :initial-element 1)))
        (bit-and foo-3 foo-2))
      (#*11))

(test standard-bitvector-3
      (let ((foo-1 (make-array 2 :element-type 'bit :initial-element 0))
            (foo-2 (make-array 2 :element-type 'bit :initial-element 1)))
        (values (bit-and foo-2 foo-1 t) foo-2))
      (#*00 #*00))

(test standard-bitvector-4
      (let ((foo-3 (make-array 2 :element-type 'bit :initial-element 1))
            (foo-2 (make-array 2 :element-type 'bit :initial-element 1))
            (foo-1 (make-array 2 :element-type 'bit :initial-element 0)))
        (values (bit-and foo-3 foo-2 foo-1) foo-1))
      (#*11 #*11))

;;; Second md bitvectors

(test standard-mdbit-1
      (let ((foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1)))
        (bit-and foo-1 foo-2))
      (#2a((0 0) (0 0))))

(test standard-mdbit-2
      (let ((foo-3 (make-array (list 2 2) :element-type 'bit :initial-element 1))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1)))
        (bit-and foo-3 foo-2))
      (#2a((1 1) (1 1))))

(test standard-mdbit-3
      (let ((foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1)))
        (values (bit-and foo-2 foo-1 t) foo-2))
      (#2A((0 0) (0 0)) #2A((0 0) (0 0))))

(test standard-mdbit-4
      (let ((foo-3 (make-array (list 2 2) :element-type 'bit :initial-element 1))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1))
            (foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0)))
        (values (bit-and foo-3 foo-2 foo-1) foo-1))
      (#2A((1 1) (1 1)) #2A((1 1) (1 1))))

;;; Now adjustable

(test standard-mdbit-1a
      (let ((foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0 :adjustable t))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t)))
        (bit-and foo-1 foo-2))
      (#2A((0 0) (0 0))))

(test standard-mdbit-2a
      (let ((foo-3 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t)))
        (bit-and foo-3 foo-2))
      (#2A((1 1) (1 1))))

(test standard-mdbit-3a
      (let ((foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0 :adjustable t))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t)))
        (values (bit-and foo-2 foo-1 t) foo-2))
      (#2A((0 0) (0 0)) #2A((0 0) (0 0))))

(test standard-mdbit-4a
      (let ((foo-3 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t))
            (foo-2 (make-array (list 2 2) :element-type 'bit :initial-element 1 :adjustable t))
            (foo-1 (make-array (list 2 2) :element-type 'bit :initial-element 0 :adjustable t)))
        (values (bit-and foo-3 foo-2 foo-1) foo-1))
      (#2A((1 1) (1 1)) #2A((1 1) (1 1))))

;;; now the hell, displaced bitarrays

(test hell-mdbit-1
      (let* ((basis (make-array 8 :element-type 'bit :initial-element 0))
             (a1 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 0))
             (a2 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 4)))
        (bit-and a1 a2))
      (#*0000))

(test hell-mdbit-2
      (let* ((basis (make-array 8 :element-type 'bit :initial-contents (list 0 1 0 1 0 1 0 1)))
             (a1 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 0))
             (a2 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 4)))
        (bit-and a1 a2))
      (#*0101))

;;; crashes
(test hell-mdbit-3
      (let* ((basis (make-array 8 :element-type 'bit :initial-contents (list 0 1 0 1 0 1 0 1)))
             (a1 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 0))
             (a2 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 4)))
        (values (bit-and a1 a2 t) a1))
      (#*0101 #*0101))

(test hell-mdbit-4
      (let* ((basis (make-array 12 :element-type 'bit :initial-contents (list 0 1 0 1 0 1 0 1 0 1 0 1)))
             (a1 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 0))
             (a2 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 4))
             (a3 (make-array 4 :element-type 'bit :displaced-to basis :displaced-index-offset 8)))
        (values (bit-and a1 a2 a3) a3))
      (#*0101 #*0101))

;;; If the 2 fill-pointer are different, this fails
(test hell-mdbit-5
      (LET ((V1 (MAKE-ARRAY 1 :ELEMENT-TYPE 'BIT :INITIAL-CONTENTS '(1) :FILL-POINTER 1))
            (V2 (MAKE-ARRAY 1 :ELEMENT-TYPE 'BIT :INITIAL-CONTENTS '(1) :FILL-POINTER 1)))
        (BIT-AND v1 v2))
      (#*1))

(test hell-mdbit-5a
      (LET ((V1 (MAKE-ARRAY 1 :ELEMENT-TYPE 'BIT :INITIAL-CONTENTS '(1) :FILL-POINTER 1))
            (V2 (MAKE-ARRAY 1 :ELEMENT-TYPE 'BIT :INITIAL-CONTENTS '(1) :FILL-POINTER 0)))
        (BIT-AND v1 v2))
      (#*1))
