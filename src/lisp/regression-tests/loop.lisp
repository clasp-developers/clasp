(in-package #:clasp-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (safety 3))))

(defmacro wrap-loop-as-in-ansi-tests (&body body)
  `(let ()
     (progn
       ,@body)))

;;; seems to work now
(test loop-complex-1
      (equal (complex 10 14)
             (wrap-loop-as-in-ansi-tests
              (LOOP FOR I FROM 1 TO 4 SUM (COMPLEX I (1+ I)) OF-TYPE COMPLEX))))

;;; seems to be macroexpanded wrong (x is declared float), errors in ansi-test- but not here, strange
(test loop-float-1
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR X FROM 1 to 4.0 COLLECT X)))

(test loop-float-2
      (wrap-loop-as-in-ansi-tests
       (LOOP FOR X FROM 1 BELOW 5.01 COLLECT X)))

;;; (DECLARE (TYPE (INTEGER 1 5) X))
;;; works, but perhaps x is temporarily 6
(test loop-fixnum-1
      (equalp '(1 2 3 4 5)
              (wrap-loop-as-in-ansi-tests
               (LOOP FOR X OF-TYPE (INTEGER 1 5) FROM 1 TO 5 COLLECT X))))

(test loop-finally-1
      (= 5 (LOOP FOR X FROM 1 TO 5 DO (PROGN) FINALLY (RETURN X))))

(test loop-finally-2
      (= 4 (LOOP FOR X FROM 1 BELOW 5 DO (PROGN) FINALLY (RETURN X))))

(test loop-finally-3
      (= 0 (LOOP FOR X FROM 10 DOWNTO 0 DO (PROGN) FINALLY (RETURN X))))

(test loop-finally-4
      (= 0 (LOOP FOR X FROM 10 ABOVE 0 DO (PROGN) FINALLY (RETURN X))))

(test loop-collect-1
      (equalp
       (list #C(0 1) #C(1 1) #C(2 1) #C(3 1) #C(4 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C FROM #C(0 1) COLLECT C))))

(test loop-collect-2
      (equalp
       (list #C(0 1) #C(2 1) #C(4 1) #C(6 1) #C(8 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C FROM #C(0 1) BY 2 COLLECT C))))

(test loop-collect-3
      (equalp
       (list #C(5 1) #C(4 1) #C(3 1) #C(2 1) #C(1 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C DOWNFROM #C(5 1) COLLECT C))))

(test loop-collect-4
      (equalp
       (list #C(10 1) #C(8 1) #C(6 1) #C(4 1) #C(2 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C DOWNFROM #C(10 1) BY 2 COLLECT C))))

(test loop-collect-5
      (equalp
       (list #C(0 1) #C(1 1) #C(2 1) #C(3 1) #C(4 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C UPFROM #C(0 1) COLLECT C))))

(test loop-collect-6
      (equalp
       (list #C(0 1) #C(2 1) #C(4 1) #C(6 1) #C(8 1))
       (wrap-loop-as-in-ansi-tests
        (LOOP FOR I FROM 1 TO 5 FOR C UPFROM #C(0 1) BY 2 COLLECT C))))

