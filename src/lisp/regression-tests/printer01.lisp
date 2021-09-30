(in-package #:clasp-tests)

(test print-1
      (LET ((*PRINT-BASE* 16))
        (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (write -1)))
      ("-1"))

(test print-1a
      (LET ((*PRINT-BASE* 16))
        (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
          (write most-negative-fixnum)))
      ("-2000000000000000"))

(test print-2 (pprint 23) ())

(test print-3 (PRINT-UNREADABLE-OBJECT (nil t)) (nil))

(test print-4
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
          (length (write-to-string obj))))
      (10))

(test print-5
      (WRITE-TO-STRING
       (MAKE-ARRAY '(4)
                   :INITIAL-CONTENTS
                   '(0 1 2 3)
                   :ELEMENT-TYPE
                   '(UNSIGNED-BYTE 2)
                   :FILL-POINTER
                   0)
       :READABLY NIL :ARRAY T :PRETTY NIL)
      ("#()"))

(test print-5a
      (let ((*print-pretty* nil)
            (*print-array* t)
            (*print-readably* nil))
        (write-to-string
         (make-array 3 :initial-contents '(1 2 3) :fill-pointer t)))
      ("#(1 2 3)"))

;;; fails to write readably
;;; http://www.lispworks.com/documentation/HyperSpec/Body/t_rnd_st.htm#random-state
;;; It can be printed out and successfully read back in by the same implementation, 
;;; Implementations are required to provide a read syntax for objects of type random-state,
;;; but the specific nature of that syntax is implementation-dependent.
;;; this would work
;;; (CORE:RANDOM-STATE-set *random-state* (CORE:RANDOM-STATE-GET *random-state*))
;;; So we perhaps need a reader-macro for random-state

(test-type print-read-1
    (read-from-string (WRITE-TO-STRING (MAKE-RANDOM-STATE *RANDOM-STATE*) :READABLY T))
    random-state)

(test-expect-error print-6 (write-byte) :type program-error)
(test-expect-error print-7 (write-byte 233 nil) :type type-error)
(test-expect-error print-8 (write-byte 233 t) :type type-error)
(test-expect-error print-9 (write-byte 233 23) :type type-error)
(test-expect-error print-10
                   (with-output-to-string (*standard-output*)
                     (write-byte 23 *standard-output*)) :type type-error)

(test print-11
      (with-standard-io-syntax
        (LET ((A (MAKE-ARRAY NIL :INITIAL-ELEMENT 23)))
          (WRITE-TO-STRING A :READABLY NIL :ARRAY T :pretty nil)))
      ("#0A23"))
(test-true print-12
      (let ((result
             (with-standard-io-syntax
               (LET ((A (MAKE-ARRAY NIL :INITIAL-ELEMENT 23)))
                 (WRITE-TO-STRING A :READABLY NIL :ARRAY nil :pretty nil)))))
        (and (zerop (search "#<" result))
             (search ">" result))))

(test print-13
      (let ((type 'SINGLE-FLOAT)
            (number -0.0))
        (LET ((*READ-DEFAULT-FLOAT-FORMAT* TYPE))
          (FORMAT NIL "~f" number)))
      ("-0.0"))

(test print-14
      (let ((type 'SINGLE-FLOAT)
            (number -0.0))
        (LET ((*READ-DEFAULT-FLOAT-FORMAT* type))
          (PRIN1-TO-STRING number)))
      ("-0.0"))

(test-type print-15
    (write-to-string 
     (LET ((S
             (OPEN "foo.txt"
                   :DIRECTION
                   :OUTPUT
                   :IF-EXISTS
                   :SUPERSEDE
                   :ELEMENT-TYPE
                   '(UNSIGNED-BYTE 8))))
       (CLOSE S)
       s))
    string)

(test PRINT.RATIOS.RANDOM.root-cause
      (let ((*PRINT-BASE* 22)
            (*print-radix* t))
        (write-to-string -59990859179/64657108615))
      ("#22r-1212B0C39/13K5KA78B"))

(test PRINT.BIT-VECTOR.RANDOM.root-cause
      (let ((*PRINT-READABLY* t)
            (*PRINT-ARRAY* NIL))
        (write-to-string
         #*011010001010000010001110110110100000000011000100001111100000000000101111010100011100110001010100001))
      ("#*011010001010000010001110110110100000000011000100001111100000000000101111010100011100110001010100001"))

(test PRINT.RATIOS.RANDOM.root-cause.2
      (let ((num -7/16)
            (*PRINT-READABLY* T)
            (*PRINT-ARRAY* NIL) (*PRINT-BASE* 10) (*PRINT-RADIX* T)
            (*PRINT-CASE* :CAPITALIZE) (*PRINT-CIRCLE* NIL)
            (*PRINT-ESCAPE* NIL) (*PRINT-GENSYM* NIL) (*PRINT-LEVEL* 47)
            (*PRINT-LENGTH* 25) (*PRINT-LINES* 48) (*PRINT-MISER-WIDTH* NIL)
            (*PRINT-PRETTY* NIL) (*PRINT-RIGHT-MARGIN* NIL)
            (*READ-DEFAULT-FLOAT-FORMAT* 'DOUBLE-FLOAT)
            )
        (write-to-string num))
      ("#10r-7/16"))

;;; e.g. not "#A#1=(T (3) #1#)" since there is no circle
(test PRINT.VECTOR.RANDOM.1.take.1
      (search
       "#1="
       (let ((vector (vector 25 26 27))
             (*PRINT-READABLY* T)
             (*PRINT-CIRCLE* t)
             (*PRINT-PRETTY* T))
         (write-to-string vector)))
      (nil))

;;; "#1=#(25 26 #1#)"
(test PRINT.VECTOR.RANDOM.1.take.2
      (search
       "#1="
       (let ((vector (vector 25 26 27))
             (*PRINT-READABLY* T)
             (*PRINT-CIRCLE* t)
             (*PRINT-PRETTY* T))
         (setf (aref vector 2) vector)
         (write-to-string vector)))
      (0))

;;; e.g. not "#A#1=(T (2 2) #1#)"
(test PRINT.array.RANDOM.1.take.1
      (search
       "#1="
       (let ((array (make-array (list 2 2) :initial-contents '((5 6)(7 8))))
             (*PRINT-READABLY* T)
             (*PRINT-CIRCLE* t)
             (*PRINT-PRETTY* T))
         (write-to-string array)))
      (nil))

(test PRINT.array.RANDOM.1.take.2
      (search
       "#1="
       (let ((array (make-array (list 2 2) :initial-contents '((5 6)(7 8))))
             (*PRINT-READABLY* T)
             (*PRINT-CIRCLE* t)
             (*PRINT-PRETTY* T)
             )
         (setf (aref array 1 1) array)
         (write-to-string array)))
      (0))

(test PRINT.cons.RANDOM.1.take.2
      (search
       "#1="
       (let ((a (list 1 2 3)))
         (setf (cdddr a) a)
         (let ((*print-circle* t))
           (write-to-string a))))
      (0))

(test-expect-error type-errors-safety>speed
                   (locally (declare (optimize (safety 3)(speed 0)))
                     (copy-pprint-dispatch 0))
                   :type type-error)

(test read-print-consistency-arrays
      (with-standard-io-syntax
        (let ((*PRINT-CIRCLE* T)
              (*PRINT-READABLY* T)
              (*PRINT-PRETTY* T))
          (values
           (read-from-string
            (with-output-to-string (s)
              (write #2A((3 4 5)) :stream s))))))
      (#2A((3 4 5))))

(test-true write-to-string.1.simplified
      (let ((unicode-string (make-array 4 :element-type 'character
                                        :initial-contents (mapcar #'code-char (list 40340 25579 40824 28331)))))
        (string=
         (with-output-to-string (s)(write unicode-string :stream s))
         (write-to-string unicode-string))))

(test drmeister-fep-problem
      (format nil "!~36,3,'0r" 1)
      ("!001"))

(test PPRINT-DISPATCH.6-simplified
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*PRINT-PPRINT-DISPATCH* (COPY-PPRINT-DISPATCH NIL))
              (*PRINT-READABLY* NIL)
              (*PRINT-ESCAPE* NIL)
              (*PRINT-PRETTY* T))
          (LET ((F
                  #'(LAMBDA (STREAM OBJ)
                      (DECLARE (IGNORE OBJ))
                      (WRITE "ABC" :STREAM STREAM))))
            (SET-PPRINT-DISPATCH '(EQL X) F)
            (WRITE-TO-STRING 'X))))
      ("ABC"))

(test PPRINT-DISPATCH.6
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*PRINT-PPRINT-DISPATCH* (COPY-PPRINT-DISPATCH NIL))
              (*PRINT-READABLY* NIL)
              (*PRINT-ESCAPE* NIL)
              (*PRINT-PRETTY* T))
          (LET ((F
                  #'(LAMBDA (STREAM OBJ)
                      (DECLARE (IGNORE OBJ))
                      (WRITE "ABC" :STREAM STREAM))))
            (VALUES (WRITE-TO-STRING 'X)
                    (SET-PPRINT-DISPATCH '(EQL X) F)
                    (WRITE-TO-STRING 'X)
                    (SET-PPRINT-DISPATCH '(EQL X) NIL)
                    (WRITE-TO-STRING 'X)))))
      ("X" nil "ABC" nil "X"))

;;; Test c++ cl:format chokes on ~2% and returns "Could not format ..."
(test lisp-format-works
      (format nil "Bar~2%")
      ("Bar

"))

(defun print-symbol-with-prefix (stream symbol &optional colon at)
  "For use with ~/: Write SYMBOL to STREAM as if it is not accessible from
  the current package."
  (declare (ignore colon at))
  (let ((*package* (find-package :keyword)))
    (write symbol :stream stream :escape t)))

(test issue-911
 (let ((count 42)
       (function 'print))
   (format nil
           "~@<~@(~D~) call~:P to ~
               ~/clasp-tests::print-symbol-with-prefix/ ~
               ~2:*~[~;was~:;were~] compiled before a compiler-macro ~
               was defined for it. A declaration of NOTINLINE at the ~
               call site~:P will eliminate this warning, as will ~
               defining the compiler-macro before its first potential ~
               use.~@:>"
           count
           function)
   t)
      (t))

(test format.justify.37
      ;; no padding, output fits
      (format nil "AA~4T~8,,,'*<~%BBBB~,12:;CCCC~;DDDD~>")
      ("AA  CCCCDDDD"))

(test format.justify.38
      ;; no padding, output doesn't fit
      (format nil "AA~4T~8,,,'*<~%BBBB~,11:;CCCC~;DDDD~>")
      ("AA  
BBBBCCCCDDDD"))

(test format.justify.39
      ;; one padding character per segment, output fits
      (format nil "AA~4T~10,,,'*<~%BBBB~,14:;CCCC~;DDDD~>")
      ("AA  CCCC**DDDD"))

(test format.justify.40
      ;; one padding character per segment, output doesn't fit
      (format nil "AA~4T~10,,,'*<~%BBBB~,13:;CCCC~;DDDD~>")
      ("AA  
BBBBCCCC**DDDD"))

(test format.justify.41
      ;; Same with ~@T
      (format nil "AA~1,2@T~8,,,'*<~%BBBB~,12:;CCCC~;DDDD~>")
      ("AA  CCCCDDDD"))

(test format.justify.42
      (format nil "AA~1,2@T~8,,,'*<~%BBBB~,11:;CCCC~;DDDD~>")
      ("AA  
BBBBCCCCDDDD"))

(test format.justify.43
      (format nil "AA~1,2@T~10,,,'*<~%BBBB~,14:;CCCC~;DDDD~>")
      ("AA  CCCC**DDDD"))

(test format.justify.44
      (format nil "AA~1,2@T~10,,,'*<~%BBBB~,13:;CCCC~;DDDD~>")
      ("AA  
BBBBCCCC**DDDD"))

(test print-0-strange-radix
      (LET ((*PRINT-READABLY* NIL))
        (LET ((*PRINT-BASE* 7))
          (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
            (PRIN1 0))))
      ("0"))

(test pprint-failure-2a
      ;; should only find  ".." once in the output, not twice
      (let ((output 
              (let ((*package* (find-package :cl-user)))
                (with-output-to-string (stream)
                  (with-standard-io-syntax
                    (let ((the-list (let ((*package* (find-package :cl-user)))(read-from-string "(this prints wrongly)"))))
                      (write the-list :pretty t  :circle t :lines 1 :right-margin 15 :readably nil :stream stream)))))))
        (count #\. output))
      (2))

(test pprint-failure-2b
      ;; should only find  ".." once in the output, not twice
      (let ((output 
              (let ((*package* (find-package :cl-user)))
                (with-output-to-string (stream)
                  (with-standard-io-syntax
                    (let ((the-list
                            (read-from-string "(this prints wrongly)")))
                      (write the-list :pretty t  :circle t :lines 1
                                      :right-margin 15 :readably nil
                                      :stream stream)))))))
        (count #\. output))
      (2))

(test pprint-failure-2
      (count #\.
             (with-output-to-string (stream)
               (with-standard-io-syntax
                 (write `(1234 12345 1234567)
                        :pretty t  :circle t :lines 1 :right-margin 15 :readably nil :stream stream))))
      (2))
(test pprint-failure-3a
      (count #\.
             (let ((*package* (find-package :cl-user)))
               (with-output-to-string (stream)
                 (with-standard-io-syntax
                   (write #(PROGV PROGV PROGV PROGV PROGV PROGV PROGV PROGV PROGV PROGV PROGV)
                          :ESCAPE T :PRETTY T :CIRCLE T :LINES 0 :RIGHT-MARGIN 28 :readably nil :stream stream)))))
      (2))

(test pprint-failure-3b
      (count #\.
             (with-output-to-string (stream)
               (with-standard-io-syntax
                 (write #(12345 12345 12345 12345 12345 12345 12345 12345 12345 12345 12345)
                        :ESCAPE T :PRETTY T :CIRCLE T :LINES 1 :RIGHT-MARGIN 28 :readably nil :stream stream))))
      (2))

(test pprint-failure-3c
      (count #\.
             (with-output-to-string (stream)
               (with-standard-io-syntax
                 (write #(1 1)
                        :ESCAPE T :PRETTY T :CIRCLE T :LINES 1 :RIGHT-MARGIN 1 :readably nil :stream stream))))
      (2))

(test write-string-happy-path
      (with-output-to-string (stream)
         ;;; SimpleBaseString_O
        (write-string
         (MAKE-ARRAY 2
                     :INITIAL-CONTENTS (list (code-char 65) (code-char 66))
                     :ELEMENT-TYPE 'base-char)
         stream)
         ;;; Str8Ns_O
        (write-string
         (MAKE-ARRAY 2
                     :INITIAL-CONTENTS (list (code-char 65) (code-char 66))
                     :adjustable t
                     :ELEMENT-TYPE 'base-char)
         stream)
         ;;;  SimpleCharacterString_O
        (write-string 
         (MAKE-ARRAY 2
                     :INITIAL-CONTENTS
                     (list (code-char 256) (code-char 256))
                     :ELEMENT-TYPE 'character)
         stream)
         ;;;  StrWNs_O
        (write-string
         (MAKE-ARRAY 2
                     :INITIAL-CONTENTS
                     (list (code-char 256) (code-char 256))
                     :adjustable t
                     :ELEMENT-TYPE 'character)
         stream)
        (write-string " » " stream))
      ("ABABĀĀĀĀ » "))

(let ((string "1234567890"))
  (with-output-to-string (stream)
    (test-expect-error write-string-error-1
                       (write-string string stream :start nil) :type type-error)
    (test-expect-error write-string-error-2
                       (write-string string stream :start -1) :type type-error)
    (test-expect-error write-string-error-2a
                       (write-string string stream :start 100) :type type-error)
    (test-expect-error write-string-error-3
                       (write-string string stream :start (make-hash-table)) :type type-error)
    (test-expect-error write-string-error-4
                       (write-string string stream :end -1) :type type-error)
    (test-expect-error write-string-error-4a
                       (write-string string stream :end 100) :type type-error)
    (test-expect-error write-string-error-5
                       (write-string string stream :end (make-hash-table)) :type type-error)
    (test-expect-error write-string-error-6
                       (write-string string stream :start 2 :end 1) :type type-error)))

(test-true write-string-subseq-SimpleBaseString_O
      (let ((string "12345")
            (from 1)
            (to 4))
        (string= (subseq string from to)
                 (with-output-to-string (stream)
                   (write-string string stream :start from :end to)))))

(test-true write-string-subseq-SimpleCharacterString_O
      (let ((string "12»45")
            (from 1)
            (to 4))
        (string= (subseq string from to)
                 (with-output-to-string (stream)
                   (write-string string stream :start from :end to)))))

(test-true write-string-subseq-Str8Ns_O
      (let ((string (MAKE-ARRAY 5
                                :INITIAL-CONTENTS (list #\1 #\2 #\3 #\4 #\5)
                                :adjustable t
                                :ELEMENT-TYPE 'base-char))
            (from 1)
            (to 4))
        (string= (subseq string from to)
                 (with-output-to-string (stream)
                   (write-string string stream :start from :end to)))))

(test-true write-string-subseq-StrWNs_O
      (let ((string (MAKE-ARRAY 5
                                :INITIAL-CONTENTS (list #\1 #\2 (char "12»45" 2) #\4 #\5)
                                :adjustable t
                                :ELEMENT-TYPE 'base-char))
            (from 1)
            (to 4))
        (string= (subseq string from to)
                 (with-output-to-string (stream)
                   (write-string string stream :start from :end to)))))

(test ansi-test-format-e
      (FORMAT NIL "~,2,,2e" 0.05)
      ("50.0e-3"))
