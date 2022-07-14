(in-package #:clasp-tests)

(defparameter *nil-bit-array-zero* (make-array nil :element-type 'bit :initial-element 0))
(defparameter *nil-bit-array-one*  (make-array nil :element-type 'bit :initial-element 1))

(test-true TEST-BITARRAY-0 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-AND (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-1 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-AND (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-2 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-AND (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-3 (EQUALP *nil-bit-array-one* (APPLY 'BIT-AND (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-4 (EQUALP #*00000 (APPLY 'BIT-AND (list #*00000 #*11111))))
(test-true TEST-BITARRAY-5 (EQUALP #*00000 (APPLY 'BIT-AND (list #*01010 #*10101))))
(test-true TEST-BITARRAY-6 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-IOR (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-7 (EQUALP *nil-bit-array-one* (APPLY 'BIT-IOR (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-8 (EQUALP *nil-bit-array-one* (APPLY 'BIT-IOR (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-9 (EQUALP *nil-bit-array-one* (APPLY 'BIT-IOR (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-10 (EQUALP #*11111 (APPLY 'BIT-IOR (list #*00000 #*11111))))
(test-true TEST-BITARRAY-11 (EQUALP #*11111 (APPLY 'BIT-IOR (list #*01010 #*10101))))
(test-true TEST-BITARRAY-12 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-XOR (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-13 (EQUALP *nil-bit-array-one* (APPLY 'BIT-XOR (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-14 (EQUALP *nil-bit-array-one* (APPLY 'BIT-XOR (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-15 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-XOR (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-16 (EQUALP #*11111 (APPLY 'BIT-XOR (list #*00000 #*11111))))
(test-true TEST-BITARRAY-17 (EQUALP #*11111 (APPLY 'BIT-XOR (list #*01010 #*10101))))
(test-true TEST-BITARRAY-18 (EQUALP *nil-bit-array-one* (APPLY 'BIT-EQV (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-19 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-EQV (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-20 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-EQV (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-21 (EQUALP *nil-bit-array-one* (APPLY 'BIT-EQV (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-22 (EQUALP #*00000 (APPLY 'BIT-EQV (list #*00000 #*11111))))
(test-true TEST-BITARRAY-23 (EQUALP #*00000 (APPLY 'BIT-EQV (list #*01010 #*10101))))
(test-true TEST-BITARRAY-24 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NAND (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-25 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NAND (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-26 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NAND (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-27 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NAND (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-28 (EQUALP #*11111 (APPLY 'BIT-NAND (list #*00000 #*11111))))
(test-true TEST-BITARRAY-29 (EQUALP #*11111 (APPLY 'BIT-NAND (list #*01010 #*10101))))
(test-true TEST-BITARRAY-30 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NOR (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-31 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NOR (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-32 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NOR (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-33 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NOR (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-34 (EQUALP #*00000 (APPLY 'BIT-NOR (list #*00000 #*11111))))
(test-true TEST-BITARRAY-35 (EQUALP #*00000 (APPLY 'BIT-NOR (list #*01010 #*10101))))
(test-true TEST-BITARRAY-36 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC1 (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-37 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ANDC1 (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-38 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC1 (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-39 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC1 (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-40 (EQUALP #*11111 (APPLY 'BIT-ANDC1 (list #*00000 #*11111))))
(test-true TEST-BITARRAY-41 (EQUALP #*10101 (APPLY 'BIT-ANDC1 (list #*01010 #*10101))))
(test-true TEST-BITARRAY-42 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC2 (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-43 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC2 (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-44 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ANDC2 (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-45 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ANDC2 (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-46 (EQUALP #*00000 (APPLY 'BIT-ANDC2 (list #*00000 #*11111))))
(test-true TEST-BITARRAY-47 (EQUALP #*01010 (APPLY 'BIT-ANDC2 (list #*01010 #*10101))))
(test-true TEST-BITARRAY-48 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC1 (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-49 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC1 (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-50 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ORC1 (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-51 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC1 (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-52 (EQUALP #*11111 (APPLY 'BIT-ORC1 (list #*00000 #*11111))))
(test-true TEST-BITARRAY-53 (EQUALP #*10101 (APPLY 'BIT-ORC1 (list #*01010 #*10101))))
(test-true TEST-BITARRAY-54 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC2 (list *nil-bit-array-zero* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-55 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-ORC2 (list *nil-bit-array-zero* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-56 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC2 (list *nil-bit-array-one* *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-57 (EQUALP *nil-bit-array-one* (APPLY 'BIT-ORC2 (list *nil-bit-array-one* *nil-bit-array-one*))))
(test-true TEST-BITARRAY-58 (EQUALP #*00000 (APPLY 'BIT-ORC2 (list #*00000 #*11111))))
(test-true TEST-BITARRAY-59 (EQUALP #*01010 (APPLY 'BIT-ORC2 (list #*01010 #*10101))))
(test-true TEST-BITARRAY-60 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NOT (list *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-61 (EQUALP *nil-bit-array-one* (APPLY 'BIT-NOT (list *nil-bit-array-zero*))))
(test-true TEST-BITARRAY-62 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NOT (list *nil-bit-array-one*))))
(test-true TEST-BITARRAY-63 (EQUALP *nil-bit-array-zero* (APPLY 'BIT-NOT (list *nil-bit-array-one*))))
(test-true TEST-BITARRAY-64 (EQUALP #*11111 (APPLY 'BIT-NOT (list #*00000))))
(test-true TEST-BITARRAY-65 (EQUALP #*10101 (APPLY 'BIT-NOT (list #*01010))))


;;; displaced arrays
;;; bit-and
(test test-bitarray-bitand-displaced-1
      (bit-and
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*00))

(test test-bitarray-bitand-displaced-2
      (bit-and
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*00))

(test test-bitarray-bitand-displaced-3
      (bit-and
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*00))

(test test-bitarray-bitand-displaced-4
      (bit-and
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*00))
;;; bit-andc1
(test test-bitarray-bitandc1-displaced-1
      (bit-andc1
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bitandc1-displaced-2
      (bit-andc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*00))

(test test-bitarray-bitandc1-displaced-3
      (bit-andc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*00))

(test test-bitarray-bitandc1-displaced-4
      (bit-andc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*00))

;;; bit-andc2
(test test-bitarray-bitandc2-displaced-1
      (bit-andc2
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*00))

(test test-bitarray-bitandc2-displaced-2
      (bit-andc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*01))

(test test-bitarray-bitandc2-displaced-3
      (bit-andc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bitandc2-displaced-4
      (bit-andc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))
;;; bit-eqv
(test test-bitarray-biteqv-displaced-1
      (BIT-EQV
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-biteqv-displaced-2
      (BIT-EQV
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*10))

(test test-bitarray-biteqv-displaced-3
      (BIT-EQV
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-biteqv-displaced-4
      (BIT-EQV
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))

;;; bit-ior bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bitior-displaced-1
      (bit-ior
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bitior-displaced-2
      (bit-ior
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*01))

(test test-bitarray-bitior-displaced-3
      (bit-ior
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bitior-displaced-4
      (bit-ior
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))
;;; bit-nand bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bitnand-displaced-1
      (bit-nand
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*11))

(test test-bitarray-bitnand-displaced-2
      (bit-nand
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*11))

(test test-bitarray-bitnand-displaced-3
      (bit-nand
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*11))

(test test-bitarray-bitnand-displaced-4
      (bit-nand
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*11))
;;; bit-nor bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bit-nor-displaced-1
      (bit-nor
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-bit-nor-displaced-2
      (bit-nor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*10))

(test test-bitarray-bit-nor-displaced-3
      (bit-nor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-bit-nor-displaced-4
      (bit-nor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))
;;; bit-orc1 bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bit-orc1-displaced-1
      (bit-orc1
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*11))

(test test-bitarray-bit-orc1-displaced-2
      (bit-orc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*10))

(test test-bitarray-bit-orc1-displaced-3
      (bit-orc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-bit-orc1-displaced-4
      (bit-orc1
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))
;;; bit-orc2 bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bit-orc2-displaced-1
      (bit-orc2
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-bit-orc2-displaced-2
      (bit-orc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*11))

(test test-bitarray-bit-orc2-displaced-3
      (bit-orc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*11))

(test test-bitarray-bit-orc2-displaced-4
      (bit-orc2
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*11))
;;; bit-xor bit-array1 bit-array2 &optional opt-arg	 resulting-bit-array
(test test-bitarray-bit-xor-displaced-1
      (bit-xor
       #*00
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bit-xor-displaced-2
      (bit-xor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       #*00)
      (#*01))

(test test-bitarray-bit-xor-displaced-3
      (bit-xor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))

(test test-bitarray-bit-xor-displaced-4
      (bit-xor
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*01))
;;; bit-not bit-array &optional opt-arg    resulting-bit-array
(test test-bitarray-bit-not-displaced-1
      (bit-not (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2))
      (#*10))

(test test-bitarray-bit-not-displaced-4
      (bit-not
       (make-array 2 :element-type 'bit :displaced-to #*0101 :displaced-index-offset 2)
       (make-array 2 :element-type 'bit :displaced-to #*0000 :displaced-index-offset 2))
      (#*10))
