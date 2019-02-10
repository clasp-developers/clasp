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

  


