(in-package #:clasp-tests)

(test-true read-from-string0
           (eq
            (read-from-string (concatenate 'string "abc" (string #\nul) "AA"))
            (intern (concatenate 'string "ABC" (string #\nul) "AA"))))

(test read-1
      (let ((result nil))
        (dolist (type (list 'short-float 'single-float 'double-float 'long-float) (reverse result))
          (let ((*read-default-float-format* type))
            (push (read-from-string "1.111") result))))
      ((1.111 1.111 1.111d0 1.111d0)))

(test read-2
      (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
        (with-output-to-string (*standard-output*)
          (let ((*read-default-float-format* 'single-float)
                (*print-readably* nil))
            (print (read-from-string (format nil "12~40,2f" most-positive-single-float))))))
      ("
#.ext:single-float-positive-infinity "))

(test-true read-3
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (string-equal
              (concatenate 'string (string #\Newline)
                           "#.ext:double-float-positive-infinity ")
              (with-output-to-string (*standard-output*)
                (let ((*read-default-float-format* 'double-float)
                      (*print-readably* nil))
                  (print (read-from-string (format nil "12~308,2f" most-positive-double-float))))))))


;;; Reader-errors

(test-expect-error read-4 (READ-FROM-STRING ")") :type reader-error)
(test-expect-error read-5 (READ-FROM-STRING ",1") :type reader-error)
(test-expect-error read-6 (READ-FROM-STRING ",") :type error)
(test-expect-error read-7 (READ-FROM-STRING "#)" NIL NIL) :type reader-error)

(test-true read-8
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
(test error-mcclim-1 (list :\.) ((:\.)))

(test-expect-error read-14
                   (WITH-INPUT-FROM-STRING (S "") (READ S))
                   :type END-OF-FILE)

(test read-15
      (LET ((RT (COPY-READTABLE)))
        (SETF (READTABLE-CASE RT) :INVERT))
      (:invert))

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
      (let ((*read-default-float-format* 'single-float))
        (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace nil))
      (123.1212 9))

(test read-from-string-1b
      (let ((*read-default-float-format* 'single-float))
        (read-from-string "123.1212 (1 2 3)" nil :end :preserve-whitespace t))
      (123.1212 8))

(test read-from-string-1c
      (let ((*read-default-float-format* 'single-float))
        (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace nil))
      (3 5))

(test read-from-string-1d
      (let ((*read-default-float-format* 'single-float))
        (read-from-string " 1 3 5" t nil :start 2 :preserve-whitespace t))
      (3 4))

(test-type issue-678 (first (list 1.e-7)) float)

(test-expect-error READ-BYTE.ERROR.3.simplyfied
                   (PROGN
                     (LET ((S (OPEN "foo.txt" :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)))
                       (CLOSE S))
                     (LET ((S (OPEN "foo.txt" :DIRECTION :INPUT)))
                       (UNWIND-PROTECT (READ-BYTE S) (CLOSE S))))
                   :type (not end-of-file))

(test-type READ-BYTE.ERROR.4.simplyfied
    (write-to-string
     (LET ((S (OPEN "foo.txt" :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)))
       (CLOSE S)
       s))
    string)

(test FILE-LENGTH.3.simplyfied
      (first  
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
             (close is)))))
      (17))

(test-type READ-SYMBOL.11 (READ-FROM-STRING "\\.") symbol)
(test glsl-toolkit-grammar.lisp-1 '|.| (\.))

(test-expect-error SYNTAX.DOT-ERROR.2 (read-from-string "..") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.3 (read-from-string "...") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.5 (READ-FROM-STRING "(1 ..)") :type reader-error)
(test-expect-error SYNTAX.DOT-ERROR.6 (READ-FROM-STRING "(1 .. 2)") :type reader-error)

(test-type READ-SYMBOL.12
    (read-from-string
     (MAKE-ARRAY 2
                 :ELEMENT-TYPE
                 'BASE-CHAR
                 :INITIAL-CONTENTS
                 (LIST #\\ #\.)))
    symbol)

(test-type READ-SYMBOL.19 (READ-FROM-STRING "123||") symbol)

(test-type READ-SYMBOL.23
    (LET ((*READ-BASE* 36)) (READ-FROM-STRING "a."))
    symbol)

(test-type READ-SYMBOL.23a
    (LET ((*READ-BASE* 36)) (READ-FROM-STRING "+a."))
    symbol)

(test-type READ-SYMBOL.23b
    (LET ((*READ-BASE* 36)) (READ-FROM-STRING "+a.a"))
    symbol)

(test READ-FROM-STRING-2a (READ-FROM-STRING "-10.") (-10 4))
(test READ-FROM-STRING-2b (READ-FROM-STRING "+10.") (10 4))
(test READ-FROM-STRING-2c (READ-FROM-STRING "+2") (2 2))
(test READ-FROM-STRING-2d (READ-FROM-STRING "-2") (-2 2))

;;; This is real bad, the letters are legal numbers, but not with the dot
;;; Is probably parsed as a float
;;; How do I distinguish exponents or other letters?
(test-type read-symbol.letters.letters
      (let ((*READ-BASE* 16)) (read-from-string "AAAA.BBB"))
      symbol)

(test-type PRINT.ARRAY.0.11.simplified
    (let ((*read-base* 18)
          (*READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT))
      (READ-FROM-STRING "0.0f0"))
    single-float)

;;; This is from ansi-tests but it's commented out there as well.
;;; The reason is that an unknown reader macro has totally unknown syntax,
;;; so the reader can't proceed except with a wild guess.
#+(or)
(test READ-SUPPRESS.17
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#garbage")))
      (nil 8))

(test READ-SUPPRESS.SHARP-ASTERISK.1
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#*")))
      (nil 2))

(test clhs-23.2.16
      (let ((*read-suppress* t))
        (mapcar #'read-from-string
                '("#(foo bar baz)" "#P(:type :lisp)" "#c1.2"
                  "#.(PRINT 'FOO)" "#3AHELLO" "#S(INTEGER)"
                  "#*ABC" "#\\GARBAGE" "#RALPHA" "#3R444")))
      ((NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

(test READ-SUPPRESS.SHARP-ASTERISK.2
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#0*")))
      (nil 3))

(test READ-SUPPRESS.SHARP-ASTERISK.10
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#1*")))
      (nil 3))

(test READ-SUPPRESS.SHARP-ASTERISK.11
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#10000*")))
      (nil 7))

(test READ-SUPPRESS.SHARP-ASTERISK.12
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#10000000000000*")))
      (nil 16))

(test READ-SUPPRESS.SHARP-ASTERISK.14
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#2*")))
      (nil 3))

(test READ-SUPPRESS.SHARP-B.5
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#0b0")))
      (nil 4))

(test READ-SUPPRESS.SHARP-R.6
      (WITH-STANDARD-IO-SYNTAX
        (LET ((*READ-SUPPRESS* T))
          (READ-FROM-STRING "#0r0")))
      (nil 4))

(test-type issue-382-a
    (let ((*readtable* (copy-readtable nil)))
      (read-from-string "#a(T 1 (2))"))
    array)

(test-type issue-382-b
    (let ((*readtable* (copy-readtable nil)))
      (read-from-string "#a(T (1) (2))"))
    array)

(test-type issue-382-c
    (let ((*readtable* (copy-readtable nil)))
      (read-from-string "#a(T (2 2) ((1 2)(3 4)))"))
    array)

(test-type issue-382-d
    (let ((*readtable* (copy-readtable nil)))
      (read-from-string "#2A((0 1 5) (foo 2 (hot dog)))"))
    array)

(defstruct %foo% %bar%)

(test-true sharp-s-new-readtable
           (let ((*readtable* (copy-readtable nil)))
             (%foo%-p (read-from-string "#S(%FOO% :%BAR% 1)"))))

(test-true SET-SYNTAX-FROM-CHAR-TRAIT-X-1a
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

(test-true SET-SYNTAX-FROM-CHAR-TRAIT-X-1b
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
(test-expect-error SYNTAX.SHARP-COLON.ERROR.1
                   (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING "#:a:b"))
                   :type reader-error)

(test SYNTAX.SHARP-B.5 (READ-FROM-STRING "#b010001/100") (17/4 12))

(test SYNTAX.SHARP-O.7 (READ-FROM-STRING "#O11/10") (9/8 7))

(test SYNTAX.SHARP-X.15 (READ-FROM-STRING "#x10/11") (16/17 7))

(test SYNTAX.SHARP-X.15a (READ-FROM-STRING "#3r10/11") (3/4 8))

;;; Reading ratios does not seem to respect *print-base*
(test read-ratio-print-base-1
      (let ((*read-base* 16))
        (with-input-from-string (stream "10/11")
          (read stream t nil nil)))
      (16/17))

;;; SYNTAX.NUMBER-TOKEN.ERROR.1 (READ-FROM-STRING "1/0") gives a division by 0, but that seems ok 

(test-expect-error SYNTAX.DOT-ERROR.4 (READ-FROM-STRING "#( . 1)" )
                   :type reader-error)

(test-expect-error SYNTAX.DOT-ERROR.7 (READ-FROM-STRING "#(1 . 2)")
                   :type reader-error)

(test-type asdf-screw-up #(#\Return #\Linefeed) vector)

(test-type SYNTAX.DOT-TOKEN.1
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING "\\."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.2
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING ".\\."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.3
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING "\\.."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.4
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING "..\\."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.5
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING ".\\.."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.6
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING "\\..."))
    symbol)

(test-type SYNTAX.DOT-TOKEN.7
    (WITH-STANDARD-IO-SYNTAX (READ-FROM-STRING ".||"))
    symbol)

;;; the underlying error is that the ratio-printing is wrong (now fixed)

(test-type PRINT.RATIOS.RANDOM.simplified
    (let ((*read-base* 10))
      (let ((ratio (read-from-string "2140/969")))
        (let ((*read-base* 4)
              (*print-base* 4))
          (read-from-string (write-to-string ratio)))))
    ratio)

(test-type PRINT.RATIOS.RANDOM.simplified.floats
    (let ((*read-base* 4))
      (read-from-string "2140.969"))
    float)

(test-type PRINT.SHORT-FLOAT.RANDOM.simplyfied
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (let ((*read-base* 7))
               (read-from-string "2.8821837e-39")))
           float)

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

(test-type PRINT.SINGLE-FLOAT.RANDOM.simplyfied.1
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (let ((*READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT))
               (read-from-string "1.8218674e-39")))
           single-float)

(test-type PRINT.SINGLE-FLOAT.RANDOM.simplyfied.2
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (let ((*READ-DEFAULT-FLOAT-FORMAT* 'DOUBLE-FLOAT))
               (read-from-string "1.8218674e-39")))
           double-float)

(test-type PRINT.SINGLE-FLOAT.RANDOM.simplyfied.3
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (let ((*READ-DEFAULT-FLOAT-FORMAT* 'short-float))
               (read-from-string "1.8218674e-39")))
           short-float)

(test-type PRINT.SINGLE-FLOAT.RANDOM.simplyfied.4
           (ext:with-float-traps-masked (:invalid :overflow :underflow :divide-by-zero)
             (let ((*READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT))
               (read-from-string "1.8218674e-39")))
           LONG-FLOAT)

(test-type PRINT.RATIOS.RANDOM.simplyfied.2
    (let ((num 16/13)
          (*PRINT-READABLY* T)
          (*PRINT-RADIX* T)
          (*PRINT-BASE* 10)
          (*read-base* 10))
      (read-from-string (write-to-string num)))
    ratio)

(test SYNTAX.ESCAPED.3.simplyfied.1
      (with-standard-io-syntax
        (symbol-name  (read-from-string "\\o")))
      ("o") :test 'string=)

(test SYNTAX.ESCAPED.3.simplyfied.2
      (with-standard-io-syntax
        (symbol-name  (read-from-string "\\O")))
      ("O") :test 'string=)

(test-true readtable-1
           (and (get-dispatch-macro-character #\# #\=)
                (get-dispatch-macro-character #\# #\#)
                (get-dispatch-macro-character #\# #\I)
                (get-dispatch-macro-character #\# #\a)
                (get-dispatch-macro-character #\# #\A)
                (get-dispatch-macro-character #\# #\s)
                (get-dispatch-macro-character #\# #\S)))

(test-true readtable-2
           (let ((new (copy-readtable)))
             (and (get-dispatch-macro-character #\# #\= new)
                  (get-dispatch-macro-character #\# #\# new)
                  (get-dispatch-macro-character #\# #\I new)
                  (get-dispatch-macro-character #\# #\a new)
                  (get-dispatch-macro-character #\# #\A new)
                  (get-dispatch-macro-character #\# #\s new)
                  (get-dispatch-macro-character #\# #\S new))))

(test-true readtable-3
           (let ((new (copy-readtable nil)))
             (and (get-dispatch-macro-character #\# #\= new)
                  (get-dispatch-macro-character #\# #\# new)
                  (get-dispatch-macro-character #\# #\I new)
                  (get-dispatch-macro-character #\# #\a new)
                  (get-dispatch-macro-character #\# #\A new)
                  (get-dispatch-macro-character #\# #\s new)
                  (get-dispatch-macro-character #\# #\S new))))

(test can-read-nil-bitarray-0 (row-major-aref #0A0 0) (0))
(test can-read-nil-bitarray-1 (row-major-aref #0A1 0) (1))

(test all-char-names-can-be-read-again
      (loop for x below (min 65535 char-code-limit)
            for char = (code-char x)
            for other = (read-from-string (format nil "#\\~a" (char-name char)))
            unless (char= char other)
              collect char)
      (nil))

(test all-chars-can-be-read-again
      (loop for x below (min 65535 char-code-limit)
            for c = (code-char x)
            unless (or (not (characterp c))
                       (let ((name (char-name c)))
                         (or (null name)
                             (and (stringp name) (char= c (name-char name))))))
              collect c)
      (nil))

(test name-char.2
      (loop for s in '("RubOut" "PAGe" "BacKspace" "RetUrn" "Tab" "LineFeed"
                       "SpaCE" "NewLine")
            for c1 = (name-char (string-upcase s))
            for c2 = (name-char (string-downcase s))
            for c3 = (name-char (string-capitalize s))
            for c4 = (name-char s)
            unless (and (char= c1 c2) (char= c2 c3) (char= c3 c4))
              collect s)
      (nil))

;;; Problems with eclector https://github.com/s-expressionists/Eclector/issues/63
(defstruct syntax-test-struct-1 a b c)

(test SYNTAX.SHARP-S.3
      (let ((v (read-from-string "#s(syntax-test-struct-1 \"A\" x)")))
        (values
         (not (not (typep v 'syntax-test-struct-1)))
         (syntax-test-struct-1-a v)
         (syntax-test-struct-1-b v)
         (syntax-test-struct-1-c v)))
      (t x nil nil))

;;; https://github.com/s-expressionists/Eclector/issues/67
(test eclector-issue-67
      (WITH-INPUT-FROM-STRING (*STANDARD-INPUT* "1 2 3 ]")
        (READ-DELIMITED-LIST #\] NIL))
      ((1 2 3)))

(test-true very-long-floats-maxima
      (let (($%e 2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952605956307381323286279434907632338298807531952510190115738341879307021540891499348841675092447614606680822648001684774118537423454424371075390777449920695517027618386062613313845830007520449338265602976067371132007093287091274437470472306969772093101416928368190255151086574637721112523897844250569536967707854499699679468644549059879316368892300987931277361782154249992295763514822082698951936680331825288693984964651058209392398294887933203625094431173012381970684161404))
        (and (floatp $%e)
             (not (ext:float-nan-p $%e)))))

(test-true issue-case-sensitivity-mode
      (locally (declare (optimize (safety 3)))
        (let ()
          (readtable-case *readtable*))))

(test-true set-syntax-from-char-from-irc-clasp
      (let ((*readtable* (copy-readtable nil)))
        (set-syntax-from-char #\0 #\`)
        (eql (get-macro-character #\0) (get-macro-character #\`))))

(test-true set-macro-character-from-irc-clasp
      (let ((*readtable* (copy-readtable nil)))
        (set-macro-character #\0 (get-macro-character #\`))
        (eql (get-macro-character #\0) (get-macro-character #\`))))


