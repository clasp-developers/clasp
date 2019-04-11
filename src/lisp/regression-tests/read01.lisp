(in-package #:clasp-tests)

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
(test-expect-error read-6 (READ-FROM-STRING ",") :type reader-error)
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

(test 'SET-SYNTAX-FROM-CHAR-TRAIT-X-1a
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


