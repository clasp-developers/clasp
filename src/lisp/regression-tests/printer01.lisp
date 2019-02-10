(in-package #:clasp-tests)

(test print-1
      (string=
       "-1"
       (LET ((*PRINT-BASE* 16))
         (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (write -1)))))

(test print-1a
      (string=
       "-2000000000000000"
       (LET ((*PRINT-BASE* 16))
         (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
           (write most-negative-fixnum)))))

(test print-2
      (null
       (multiple-value-list
        (pprint 23))))

(test print-3
      (equal '(nil)
             (MULTIPLE-VALUE-LIST
              (PRINT-UNREADABLE-OBJECT (nil t)))))

(test print-4
      (= 10
         (let ((*package* (find-package "COMMON-LISP-USER"))
               (*print-array* t)
               (*print-base* 10)
               (*print-case* :upcase)
               (*print-circle* nil)
               (*print-escape* t)
               (*print-gensym* t)
               (*print-length* nil)
               (*print-level* nil)
               (*print-lines* nil)
               (*print-miser-width* nil)
               (*print-pretty* nil)
               (*print-radix* nil)
               (*print-readably* t)
               (*print-right-margin* nil)
               (*read-base* 10)
               (*read-default-float-format* 'single-float)
               (*read-eval* t)
               (*read-suppress* nil)
               )
           (let ((*print-pretty* t)
                 (*print-readably* nil)
                 (obj (vector nil nil)))
             (length (write-to-string obj))))))

(test print-5
      (string= "#()"
               (WRITE-TO-STRING
                (MAKE-ARRAY '(4)
                            :INITIAL-CONTENTS
                            '(0 1 2 3)
                            :ELEMENT-TYPE
                            '(UNSIGNED-BYTE 2)
                            :FILL-POINTER
                            0)
                :READABLY NIL :ARRAY T :PRETTY NIL)))

;;; fails to write readably
;;; http://www.lispworks.com/documentation/HyperSpec/Body/t_rnd_st.htm#random-state
;;; It can be printed out and successfully read back in by the same implementation, 
;;; Implementations are required to provide a read syntax for objects of type random-state,
;;; but the specific nature of that syntax is implementation-dependent.
;;; this would work
;;; (CORE:RANDOM-STATE-set *random-state* (CORE:RANDOM-STATE-GET *random-state*))
;;; So we perhaps need a reader-macro for random-state

(test print-read-1
      (read-from-string (WRITE-TO-STRING (MAKE-RANDOM-STATE *RANDOM-STATE*) :READABLY T)))

(test print-all-array-types
      (let ((arrays
             (list
        ;;; MDArray_O?
              (MAKE-ARRAY (list 2 2) :INITIAL-CONTENTS '((0 1)(1 0)) :ELEMENT-TYPE t)
        ;;; SimpleVector_O
              (vector 1 2 3)
        ;;;BitVector_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :adjustable t
                          :ELEMENT-TYPE 'bit)
        ;;; SIMPLE-VECTOR-BYTE8-T
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE '(integer 0 4))
        ;;; SimpleVector_byte16_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE '(integer 0 300))
        ;;; SimpleVector_byte32_t_O>
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer 0 (1+ (* 256 256))))
        ;;;SimpleVector_byte64_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer 0 (1+ most-positive-fixnum)))
        ;;; SimpleVector_fixnum_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer 0 most-positive-fixnum))
        ;;; SimpleVector_int8_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer -8 8))
        ;;; SimpleVector_int16_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer -257 257))
        ;;; SimpleVector_int32_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer (- (expt 2 17)) (expt 2 17)))
        ;;; SimpleVector_int64_t_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer (- (expt 2 31)) (1+ most-positive-fixnum)))
        ;;; SimpleVector_fixnum
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0 1)
                          :ELEMENT-TYPE (list 'integer (- (expt 2 31)) (expt 2 31)))
        ;;; SimpleVectorFloat_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0.0 1.0)
                          :ELEMENT-TYPE 'single-float)
        ;;;SimpleVectorDouble_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS '(0.0d0 1.0d0)
                          :ELEMENT-TYPE 'double-float)
        ;;; SimpleBaseString_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS (list (code-char 65) (code-char 66))
                          :ELEMENT-TYPE 'base-char)
        ;;; Str8Ns_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS (list (code-char 65) (code-char 66))
                          :adjustable t
                          :ELEMENT-TYPE 'base-char)
        ;;;  SimpleCharacterString_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS
                          (list (code-char 256) (code-char 256))
                          :ELEMENT-TYPE 'character)
        ;;;  StrWNs_O
              (MAKE-ARRAY 2
                          :INITIAL-CONTENTS
                          (list (code-char 256) (code-char 256))
                          :adjustable t
                          :ELEMENT-TYPE 'character)
              )))
        (dolist (array arrays t)
          (print (write-to-string array :READABLY nil :ARRAY nil :PRETTY NIL))
          (print (write-to-string array :READABLY nil :ARRAY t :PRETTY NIL))
          #+(or)(print (write-to-string array :READABLY t :ARRAY nil :PRETTY NIL))
          (print (write-to-string array :READABLY t :ARRAY t :PRETTY NIL))
          (format t "~%"))))


(test-expect-error print-6 (write-byte) :type program-error)
(test-expect-error print-7 (write-byte 233 nil) :type type-error)
(test-expect-error print-8 (write-byte 233 t) :type type-error)
(test-expect-error print-9 (write-byte 233 23) :type type-error)
(test-expect-error print-10
                   (with-output-to-string (*standard-output*)
                     (write-byte 23 *standard-output*)) :type type-error)

(test print-11
      (string= "#0A23"
               (with-standard-io-syntax
                 (LET ((A (MAKE-ARRAY NIL :INITIAL-ELEMENT 23)))
                   (WRITE-TO-STRING A :READABLY NIL :ARRAY T :pretty nil)))))

(test print-12
      (let ((result
             (with-standard-io-syntax
               (LET ((A (MAKE-ARRAY NIL :INITIAL-ELEMENT 23)))
                 (WRITE-TO-STRING A :READABLY NIL :ARRAY nil :pretty nil)))))
        (and (zerop (search "#<" result))
             (search ">" result))))


