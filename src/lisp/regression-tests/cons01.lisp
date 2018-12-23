(in-package #:clasp-tests)

(test adjoin-1 (equalp (list 3 1 2)(let ()(adjoin 1 (list 3 1 2)))))
(test adjoin-2 (equalp (list 5 3 1 2)(let ()(adjoin 5 (list 3 1 2)))))
(test adjoin-3 (equalp '((1 3) (2 4))
                       (let ()
                         (adjoin (list 1 2) (list (list 1 3)(list 2 4)) :key #'first))))
(test pushnew-1
      (equalp (list 3 1 2)
              (let ((x (list 3 1 2)))
                (pushnew 1 x)
                x)))
(test pushnew-2
      (equalp (list 5 3 1 2)
              (let ((x (list 3 1 2)))
                (pushnew 5 x)
                x)))

(test pushnew-3
      (equalp '((1 3) (2 4))
              (let ((x (list (list 1 3)(list 2 4))))
                (pushnew (list 1 2) x :key #'first)
                x)))

;;; whether z and y have their values seems to depend on the order key and test are executed
;;;after discussion on #clasp, it seems to be a requirement that evaluation is left to right 
(test ansi-PUSHNEW.14a
      (equalp
       '((A B C)
         (A B C)
         3
         1
         3
         2)
      (multiple-value-bind
            (a b c d e f)
          (LET ((I 0)
                X
                Y
                Z
                (D '(B C)))
            (VALUES
             (PUSHNEW
              (PROGN (SETF X (INCF I)) 'A)
              D
              :TEST
               (PROGN (SETF Z (INCF I)) #'EQL)
              :KEY
              (PROGN (SETF Y (INCF I)) #'IDENTITY))
             D
             I
             X
             Y
             Z))
        (list a b c d e f))))

(test ansi-PUSHNEW.14b
      (equalp
       '((A B C)
         (A B C)
         3
         1
         2
         3)
       (multiple-value-bind
             (a b c d e f)
           (LET ((I 0)
                 X
                 Y
                 Z
                 (D '(B C)))
             (VALUES
              (PUSHNEW
               (PROGN (SETF X (INCF I)) 'A)
               D
               :KEY
               (PROGN (SETF Y (INCF I)) #'IDENTITY)
               :TEST
               (PROGN (SETF Z (INCF I)) #'EQL)
               )
              D
              I
              X
              Y
              Z))
         (list a b c d e f))))



