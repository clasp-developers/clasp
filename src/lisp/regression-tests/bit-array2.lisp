(in-package #:clasp-tests)

;;(defparameter *nil-bit-array-zero* (make-array nil :element-type 'bit :initial-element 0))
;;(defparameter *nil-bit-array-one*  (make-array nil :element-type 'bit :initial-element 1))
(defparameter *zero-bit-array* #*00000)
(defparameter *one-bit-array* #*00001)

;; testing zerop
(test-true TEST-SBV-ZEROP (EQUALP T (APPLY 'CORE:SBV-ZEROP (list #*00000))))
(test-true TEST-SBV-ZEROP1 (EQUALP T (APPLY 'CORE:SBV-ZEROP (list #*00000000000000000000))))
(test-true TEST-SBV-ZEROP2 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*01010))))
(test-true TEST-SBV-ZEROP3 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*01010111))))
(test-true TEST-SBV-ZEROP4 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*000000000000000000000001))))
(test-true TEST-SBV-ZEROP5 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*01001101001100000110101000001011))))
(test-true TEST-SBV-ZEROP6 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*0000000000100000100001000000000000000000000000000000000000000001010101010101010101010101010101011010110101010010101010110010000001))))
(test-true TEST-SBV-ZEROP7 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list #*01))))
(test-true TEST-SBV-ZEROP8 (EQUALP NIL (APPLY 'CORE:SBV-ZEROP (list *one-bit-array*))))
(test-true TEST-SBV-ZEROP9 (EQUALP T (APPLY 'CORE:SBV-ZEROP (list *zero-bit-array*))))

;; testing position-one
(test-true TEST-SBV-POSITION-ONE (EQUALP NIL (APPLY 'CORE:SBV-POSITION-ONE (list #*00000000000))))
(test-true TEST-SBV-POSITION-ONE1 (EQUALP 0 (APPLY 'CORE:SBV-POSITION-ONE (list #*1))))
(test-true TEST-SBV-POSITION-ONE2 (EQUALP 1 (APPLY 'CORE:SBV-POSITION-ONE (list #*01))))
(test-true TEST-SBV-POSITION-ONE3 (EQUALP 10 (APPLY 'CORE:SBV-POSITION-ONE (list #*00000000001))))

;; testing sbv-bit-and
(test-true TEST-SBV-BIT-AND (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-AND (list *zero-bit-array* *zero-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-AND-1 (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-AND (list *zero-bit-array* *one-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-AND-2 (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-AND (list *one-bit-array* *zero-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-AND-3 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-AND (list *one-bit-array* *one-bit-array* #*00001 1))))
(test-true TEST-SBV-BIT-AND-4 (EQUALP #*00000 (APPLY 'CORE:SBV-BIT-AND (list #*00000 #*11111 #*00000 1))))
(test-true TEST-SBV-BIT-AND-5 (EQUALP #*00000 (APPLY 'CORE:SBV-BIT-AND (list #*01010 #*10101 #*00000 1))))

;; testing sbv-bit-ior
(test-true TEST-SBV-BIT-IOR (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-IOR (list *zero-bit-array* *zero-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-IOR-1 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-IOR (list *zero-bit-array* *one-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-IOR-2 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-IOR (list *one-bit-array* *zero-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-IOR-3 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-IOR (list *one-bit-array* *one-bit-array* #*00001 10))))
(test-true TEST-SBV-BIT-IOR-4 (EQUALP #*11111 (APPLY 'CORE:SBV-BIT-IOR (list #*00000 #*11111  #*00001 10))))
(test-true TEST-SBV-BIT-IOR-5 (EQUALP #*11111 (APPLY 'CORE:SBV-BIT-IOR (list #*01010 #*10101  #*00001 10))))

;; testing sbv-bit-xor
(test-true TEST-SBV-BIT-XOR (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-XOR (list *zero-bit-array* *zero-bit-array* #*00000 10))))
(test-true TEST-SBV-BIT-XOR-1 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-XOR (list *zero-bit-array* *one-bit-array* #*00000 10))))
(test-true TEST-SBV-BIT-XOR-2 (EQUALP *one-bit-array* (APPLY 'CORE:SBV-BIT-XOR (list *one-bit-array* *zero-bit-array* #*00000 10))))
(test-true TEST-SBV-BIT-XOR-3 (EQUALP *zero-bit-array* (APPLY 'CORE:SBV-BIT-XOR (list *one-bit-array* *one-bit-array* #*00000 10))))
(test-true TEST-SBV-BIT-XOR-4 (EQUALP #*11111 (APPLY 'CORE:SBV-BIT-XOR (list #*00000 #*11111 #*00000 10))))
(test-true TEST-SBV-BIT-XOR-5 (EQUALP #*11111 (APPLY 'CORE:SBV-BIT-XOR (list #*01010 #*10101 #*00000 10))))

;; testing sbv-bit-eqv

