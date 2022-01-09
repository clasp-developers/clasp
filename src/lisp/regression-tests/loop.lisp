(in-package #:clasp-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (safety 3))))

(defmacro wrap-loop-as-in-ansi-tests (&body body)
  `(let ()
     (progn
       ,@body)))

(test loop-complex-1
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 4 SUM (COMPLEX I (1+ I)) OF-TYPE COMPLEX))
      (#c(10 14)))

;;; is macroexpanded wrong (x is declared float), errors only with (optimize (safety 3))
(test loop-float-1
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR X FROM 1 to 4.0 COLLECT X))
      ((1 2 3 4)))

(test loop-float-2
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR X FROM 1 BELOW 5.01 COLLECT X))
      ((1 2 3 4 5)))

;;; works, but perhaps x is temporarily 6
(test loop-fixnum-1
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR X OF-TYPE (INTEGER 1 5) FROM 1 TO 5 COLLECT X))
      ((1 2 3 4 5)))

(test loop-finally-1
      (LOOP FOR X FROM 1 TO 5 DO (PROGN) FINALLY (RETURN X))
      (5))

(test loop-finally-2
      (LOOP FOR X FROM 1 BELOW 5 DO (PROGN) FINALLY (RETURN X))
      (4))

(test loop-finally-3
      (LOOP FOR X FROM 10 DOWNTO 0 DO (PROGN) FINALLY (RETURN X))
      (0))

(test loop-finally-4
      (LOOP FOR X FROM 10 ABOVE 0 DO (PROGN) FINALLY (RETURN X))
      (1))

(test loop-collect-1
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C FROM #C(0 1) COLLECT C))
      ((#C(0 1) #C(1 1) #C(2 1) #C(3 1) #C(4 1))))

(test loop-collect-2
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C FROM #C(0 1) BY 2 COLLECT C))
      ((#C(0 1) #C(2 1) #C(4 1) #C(6 1) #C(8 1))))

(test loop-collect-3
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C DOWNFROM #C(5 1) COLLECT C))
      ((#C(5 1) #C(4 1) #C(3 1) #C(2 1) #C(1 1))))

(test loop-collect-4
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C DOWNFROM #C(10 1) BY 2 COLLECT C))
      ((#C(10 1) #C(8 1) #C(6 1) #C(4 1) #C(2 1))))

(test loop-collect-5
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C UPFROM #C(0 1) COLLECT C))
      ((#C(0 1) #C(1 1) #C(2 1) #C(3 1) #C(4 1))))

(test loop-collect-6
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR I FROM 1 TO 5 FOR C UPFROM #C(0 1) BY 2 COLLECT C))
      ((#C(0 1) #C(2 1) #C(4 1) #C(6 1) #C(8 1))))

;;; from https://trac.clozure.com/ccl/ticket/1085
(test loop-stassats-ccl-1085
      (loop for x = #'(lambda ()) for y = 10 then 20 return y)
      (10))

;;; raised by beach in #clasp
(test loop-issue-1044
      (loop with (a b) = '(1 nil) for (c d) = '(2 nil)
            do (return (list a b c d)))
      ((1 nil 2 nil)))

;;; https://github.com/clasp-developers/clasp/issues/1109
(test loop.13.29
      (loop named foo for i = 1 then (return-from foo :good))
      (:good))

(test loop.13.69
      (block nil (loop named foo for i on '(a b c) by (return :good)
                       return :bad)
             :bad)
      (:good))

;;; did segfault previously
(test-expect-error
 issue-1212
 (loop for x in '(a . b) collect x)
 :type type-error)

;;; did segfault previously
(test-expect-error
 issue-1239
 (loop for x in 42
        do (format t "x = ~a~%" x))
 :type type-error)
