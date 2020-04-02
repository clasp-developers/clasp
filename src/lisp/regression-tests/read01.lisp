(in-package #:clasp-tests)

(test read-from-string0
      (eq
       (read-from-string (concatenate 'string "abc" (string #\nul) "AA"))
       (intern (concatenate 'string "ABC" (string #\nul) "AA"))))

(test read-1
      (equalp (list 1.111 1.111 1.111d0 1.111d0)
              (let ((result nil))
                (dolist (type (list 'short-float 'single-float 'double-float 'long-float) (reverse result))
                  (let ((*read-default-float-format* type))
                    (push (read-from-string "1.111") result))))))

(test read-2
      (string-equal
       "
#.ext:single-float-positive-infinity "
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'single-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~40,2f" most-positive-single-float)))))))

(test read-3
      (string-equal
       (concatenate 'string (string #\Newline)
                    "#.ext:double-float-positive-infinity ")
       (with-output-to-string (*standard-output*)
         (let ((*read-default-float-format* 'double-float)
               (*print-readably* nil))
           (print (read-from-string (format nil "12~308,2f" most-positive-double-float)))))))


;;; Reader-errors

(test-expect-error read-4 (READ-FROM-STRING ")") :type reader-error)
(test-expect-error read-5 (READ-FROM-STRING ",1") :type reader-error)
(test-expect-error read-6 (READ-FROM-STRING ",") :type error)
(test-expect-error read-7 (READ-FROM-STRING "#)" NIL NIL) :type reader-error)

(test read-8
      (let ((wide-string (make-string 4 :initial-element (code-char 256))))
        (string= wide-string
                 (with-input-from-string (stream wide-string)
                   (read-line stream)))))

(test-expect-error read-9 (read-byte) :type PROGRAM-ERROR)
(test-expect-error read-10 (read-byte nil) :type type-error)
(test-expect-error read-11 (read-byte t) :type type-error)
(test-expect-error read-12 (read-byte 23) :type type-error)
(test-expect-error read-13
                   (with-input-from-string (strm "wq12351523765127635765232")
                     (read-byte strm))
                   :type error)

;;; used to error with A string of dots was encountered by the reader.
(test error-mcclim-1
      (list :\.))

(test-expect-error read-14
                   (WITH-INPUT-FROM-STRING (S "") (READ S))
                   :type END-OF-FILE)

(test read-15
      (eq :invert (LET ((RT (COPY-READTABLE)))
                    (SETF (READTABLE-CASE RT) :INVERT))))

(test-expect-error read-16
                   (LET ((RT (COPY-READTABLE)))
                     (SETF (READTABLE-CASE RT) :pepito))
                   :type type-error)

(test-expect-error read-17-a
                   (READTABLE-CASE 23)
                   :type type-error)

(test-expect-error read-17-b
                   (READTABLE-CASE nil)
                   :type type-error)

(test read-from-string-1a
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace nil))
        (and (= object 123.1212)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 9))))

(test read-from-string-1b
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace t))
        (and (= object 123.1212)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 8))))

(test read-from-string-1c
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace nil))
        (and (= object 3)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 5))))

(test read-from-string-1d
      (multiple-value-bind
            (object position)
          (let ((*read-default-float-format* 'single-float))
            (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace t))
        (and (= object 3)
             ;;; position, is the index of the first character in the bounded string that was not read
             ;;; in this case why 9 and not 8
             (= position 4))))

(test issue-678
      (floatp (first (list 1.e-7))))

(test READ-BYTE.ERROR.3.simplyfied
      (PROGN
        (LET ((S (OPEN "foo.txt" :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)))
          (CLOSE S))
        (handler-case (LET ((S (OPEN "foo.txt" :DIRECTION :INPUT)))
                        (UNWIND-PROTECT (READ-BYTE S) (CLOSE S)))
          (end-of-file (e) nil)
          (error (e) t))))

(test READ-BYTE.ERROR.4.simplyfied
      (stringp
       (write-to-string
        (LET ((S (OPEN "foo.txt" :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)))
          (CLOSE S)
          s))))

(test FILE-LENGTH.3.simplyfied
      (= 17 (first  
             (let* ((i 9)
                    (etype  `(unsigned-byte ,i))
                    (e  (max 0 (- (ash 1 i) 5)))
                    (os (open "tmp.dat" :direction :output
                              :if-exists :supersede
                              :element-type etype)))
               (loop repeat 17 do (write-byte e os))
               (close os)
               (let ((is (open "tmp.dat" :direction :input
                               :element-type etype)))
                 (prog1
                     (list (file-length is) (stream-element-type is) e etype)
                   (close is)))))))

(test READ-SYMBOL.11 (symbolp (READ-FROM-STRING "\\.")))
#+cst (test glsl-toolkit-grammar.lisp-1 (eq '|.| '\.))

(test-expect-error SYNTAX.DOT-ERROR.2 (read-from-string "..") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.3 (read-from-string "...") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.5 (READ-FROM-STRING "(1 ..)") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.6 (READ-FROM-STRING "(1 .. 2)") :type reader-error)

(test READ-SYMBOL.12
      (symbolp
       (read-from-string
        (MAKE-ARRAY 2
                    :ELEMENT-TYPE
                    'BASE-CHAR
                    :INITIAL-CONTENTS
                    (LIST #\\ #\.)))))

(test READ-SYMBOL.19 (symbolp (READ-FROM-STRING "123||")))

(test READ-SYMBOL.23
      (symbolp
       (LET ((*READ-BASE* 36))
         (READ-FROM-STRING "a."))))

(test READ-SYMBOL.23a
      (symbolp
       (LET ((*READ-BASE* 36))
         (READ-FROM-STRING "+a."))))

(test READ-SYMBOL.23b
      (symbolp
       (LET ((*READ-BASE* 36))
         (READ-FROM-STRING "+a.a"))))

(test READ-FROM-STRING-2a (= -10 (READ-FROM-STRING "-10.")))
(test READ-FROM-STRING-2b (= 10 (READ-FROM-STRING "+10.")))
(test READ-FROM-STRING-2c (= 2 (READ-FROM-STRING "+2")))
(test READ-FROM-STRING-2d (= -2 (READ-FROM-STRING "-2")))

;;; This is real bad, the letters are legal numbers, but not with the dot
;;; Is probably parsed as a float
;;; How do I distinguish exponents or other letters?
(test read-symbol.letters.letters
      (symbolp (let ((*READ-BASE* 16))
                 (read-from-string "AAAA.BBB"))))

(test PRINT.ARRAY.0.11.simplified
      (typep 
       (let ((*read-base* 18)
             (*READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT))
         (READ-FROM-STRING "0.0f0"))
       'single-float))

(test READ-SUPPRESS.17
      (equal (list nil 8)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#garbage"))))))

(test READ-SUPPRESS.SHARP-ASTERISK.1
      (equal (list nil 2)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#*"))))))

(test clhs-23.2.16
      (equal (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
             (let ((*read-suppress* t))
               (mapcar #'read-from-string
                       '("#(foo bar baz)" "#P(:type :lisp)" "#c1.2"
                         "#.(PRINT 'FOO)" "#3AHELLO" "#S(INTEGER)"
                         "#*ABC" "#\GARBAGE" "#RALPHA" "#3R444")))))

(test READ-SUPPRESS.SHARP-ASTERISK.2
      (equal (list nil 3)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#0*"))))))

(test READ-SUPPRESS.SHARP-ASTERISK.10
      (equal (list nil 3)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#1*"))))))

(test READ-SUPPRESS.SHARP-ASTERISK.11
      (equal (list nil 7)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#10000*"))))))

(test READ-SUPPRESS.SHARP-ASTERISK.12
      (equal (list nil 16)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#10000000000000*"))))))

(test READ-SUPPRESS.SHARP-ASTERISK.14
      (equal (list nil 3)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#2*"))))))

(test READ-SUPPRESS.SHARP-B.5
      (equal (list nil 4)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#0b0"))))))

(test READ-SUPPRESS.SHARP-R.6
      (equal (list nil 4)
             (multiple-value-list
              (WITH-STANDARD-IO-SYNTAX
                (LET ((*READ-SUPPRESS* T))
                  (READ-FROM-STRING "#0r0"))))))

(test issue-382-a
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T 1 (2))"))))

(test issue-382-b
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T (1) (2))"))))

(test issue-382-c
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#a(T (2 2) ((1 2)(3 4)))"))))

(test issue-382-d
      (arrayp (let ((*readtable* (copy-readtable nil)))
                (read-from-string "#2A((0 1 5) (foo 2 (hot dog)))"))))

(defstruct %foo% %bar%)

(test sharp-s-new-readtable
      (let ((*readtable* (copy-readtable nil)))
        (%foo%-p (read-from-string "#S(%FOO% :%BAR% 1)"))))

(test SET-SYNTAX-FROM-CHAR-TRAIT-X-1a
      (let ((result
             (let ((chars (list #\Backspace #\Tab  #\Newline #\Linefeed #\Page #\Return #\Space #\Rubout)))
               ;;; 2.1.4 Character Syntax Types
               ;;; Backspace and Rubout are constituent but invalid
               (mapcar #'(lambda(c)
                           (WITH-STANDARD-IO-SYNTAX
                             (LET ((*READTABLE* (COPY-READTABLE NIL)))
                                (handler-case 
                                   (READ-FROM-STRING
                                    (CONCATENATE 'STRING (STRING C) "Z"))
                                 (reader-error (e) e)))))
                       chars))))
        (and (typep (first result) 'reader-error)
             (typep (eighth result) 'reader-error))))

(test SET-SYNTAX-FROM-CHAR-TRAIT-X-1b
      (let ((result
             (let ((chars (list #\Backspace #\Tab  #\Newline #\Linefeed #\Page #\Return #\Space #\Rubout)))
               (mapcar #'(lambda(c)
                           (WITH-STANDARD-IO-SYNTAX
                             (LET ((*READTABLE* (COPY-READTABLE NIL)))
                               (SET-SYNTAX-FROM-CHAR C #\X)
                               ;;; see 2.1.4.2 Constituent Traits
                               ;;; all these chars should fail
                               (handler-case 
                                   (READ-FROM-STRING
                                    (CONCATENATE 'STRING (STRING C) "Z"))
                                 (reader-error (e) e)))))
                       chars))))
        (every #'(lambda(a)(typep a 'reader-error)) result)))


;;; Package a: is not defined, should error
(test SYNTAX.SHARP-COLON.ERROR.1
      (WITH-STANDARD-IO-SYNTAX
        (eq :good
            (handler-case
                (READ-FROM-STRING "#:a:b")
              (reader-error (e) :good)))))

(test SYNTAX.SHARP-B.5 (= 17/4 (READ-FROM-STRING "#b010001/100")))

(test SYNTAX.SHARP-O.7 (= 9/8 (READ-FROM-STRING "#O11/10")))

(test SYNTAX.SHARP-X.15 (= 16/17 (READ-FROM-STRING "#x10/11")))

(test SYNTAX.SHARP-X.15a (= 3/4 (READ-FROM-STRING "#3r10/11")))

;;; Reading ratios does not seem to respect *print-base*
(test read-ratio-print-base-1
      (= 16/17
         (let ((*read-base* 16))
           (with-input-from-string (stream "10/11")
             (read stream t nil nil)))))

;;; SYNTAX.NUMBER-TOKEN.ERROR.1 (READ-FROM-STRING "1/0") gives a division by 0, but that seems ok 

(test SYNTAX.DOT-ERROR.4
      (handler-case (READ-FROM-STRING "#( . 1)" )
        (reader-error (e) e :good)
        (error (e) e :bad)))

(test SYNTAX.DOT-ERROR.7 
      (handler-case (READ-FROM-STRING "#(1 . 2)")
        (reader-error (e) e :good)
        (error (e) e :bad)))

(test asdf-screw-up (vectorp #(#\Return #\Linefeed)))

(test SYNTAX.DOT-TOKEN.1
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING "\\."))))

(test SYNTAX.DOT-TOKEN.2
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING ".\\."))))

(test SYNTAX.DOT-TOKEN.3
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING "\\.."))))

(test SYNTAX.DOT-TOKEN.4
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING "..\\."))))

(test SYNTAX.DOT-TOKEN.5
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING ".\\.."))))

(test SYNTAX.DOT-TOKEN.6
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING "\\..."))))

(test SYNTAX.DOT-TOKEN.7
      (symbolp
       (WITH-STANDARD-IO-SYNTAX
         (READ-FROM-STRING ".||"))))

;;; the underlying error is that the ratio-printing is wrong (now fixed)

(test PRINT.RATIOS.RANDOM.simplified
      (numberp
       (let ((*read-base* 10))
         (let ((ratio (read-from-string "2140/969")))
           (let ((*read-base* 4)
                 (*print-base* 4))
             (read-from-string (write-to-string ratio)))))))

(test PRINT.RATIOS.RANDOM.simplified.floats
      (numberp
       (let ((*read-base* 4))
         (read-from-string "2140.969"))))

(test PRINT.SHORT-FLOAT.RANDOM.simplyfied
      (numberp
       (let ((*read-base* 7))
         (read-from-string "2.8821837e-39"))))

#+kpoeck
(test
 PRINT.RATIOS.RANDOM.simplyfied
 (let ((*read-base* 10)
       (*print-base* 10))
   (= -59990859179/64657108615
      (let* ((*PRINT-BASE* 22)
             (*print-radix* t)
             (*read-base* *PRINT-BASE*))
        (read-from-string
         (write-to-string -59990859179/64657108615))))))

(test PRINT.SINGLE-FLOAT.RANDOM.simplyfied.1
      (typep
       (let ((*READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT))
         (read-from-string "1.8218674e-39"))
       'SINGLE-FLOAT))

(test PRINT.SINGLE-FLOAT.RANDOM.simplyfied.2
      (typep
       (let ((*READ-DEFAULT-FLOAT-FORMAT* 'DOUBLE-FLOAT))
         (read-from-string "1.8218674e-39"))
       'DOUBLE-FLOAT))

(test PRINT.SINGLE-FLOAT.RANDOM.simplyfied.3
      (typep
       (let ((*READ-DEFAULT-FLOAT-FORMAT* 'short-float))
         (read-from-string "1.8218674e-39"))
       'short-float))

(test PRINT.SINGLE-FLOAT.RANDOM.simplyfied.4
      (typep
       (let ((*READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT))
         (read-from-string "1.8218674e-39"))
       'LONG-FLOAT))

(test PRINT.RATIOS.RANDOM.simplyfied.2
      (numberp
       (let ((num 16/13)
             (*PRINT-READABLY* T)
             (*PRINT-RADIX* T)
             (*PRINT-BASE* 10)
             (*read-base* 10)
             )
         (read-from-string (write-to-string num)))))

(test SYNTAX.ESCAPED.3.simplyfied.1
      (with-standard-io-syntax
        (string= "o" (symbol-name  (read-from-string "\\o")))))

(test SYNTAX.ESCAPED.3.simplyfied.2
      (with-standard-io-syntax
        (string= "O" (symbol-name  (read-from-string "\\O")))))

(test readtable-1
      (and (get-dispatch-macro-character #\# #\=)
             (get-dispatch-macro-character #\# #\#)
             (get-dispatch-macro-character #\# #\I)
             (get-dispatch-macro-character #\# #\!)
             (get-dispatch-macro-character #\# #\a)
             (get-dispatch-macro-character #\# #\A)
             (get-dispatch-macro-character #\# #\s)
             (get-dispatch-macro-character #\# #\S)))

(test readtable-2
      (let ((new (copy-readtable)))
        (and (get-dispatch-macro-character #\# #\= new)
             (get-dispatch-macro-character #\# #\# new)
             (get-dispatch-macro-character #\# #\I new)
             (get-dispatch-macro-character #\# #\! new)
             (get-dispatch-macro-character #\# #\a new)
             (get-dispatch-macro-character #\# #\A new)
             (get-dispatch-macro-character #\# #\s new)
             (get-dispatch-macro-character #\# #\S new))))

(test readtable-3
      (let ((new (copy-readtable nil)))
        (and (get-dispatch-macro-character #\# #\= new)
             (get-dispatch-macro-character #\# #\# new)
             (get-dispatch-macro-character #\# #\I new)
             (get-dispatch-macro-character #\# #\! new)
             (get-dispatch-macro-character #\# #\a new)
             (get-dispatch-macro-character #\# #\A new)
             (get-dispatch-macro-character #\# #\s new)
             (get-dispatch-macro-character #\# #\S new))))

(test can-read-nil-bitarray-0 (zerop (row-major-aref #0A0 0)))
(test can-read-nil-bitarray-1 (= 1 (row-major-aref #0A1 0)))

(test all-char-names-can-be-read-again
      (dotimes (x (min 65535 char-code-limit) t)
        (let ((char (code-char x)))
          (unless (char= char
                         (handler-case
                             (read-from-string (format nil "#\\~a" (char-name char)))
                           (error (e)(values "" e))))
            (return nil)))))
