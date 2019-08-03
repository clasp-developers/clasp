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
         
(test-expect-error GET-PROPERTIES.ERROR.5 (GET-PROPERTIES '(A 1 B 2 C 3 . D) '(X Y)) :type type-error)


(test ldiff-1 (equalp '(A B C D E . F) (LDIFF ( COPY-TREE '(A B C D E . F)) 'A)))
(test-expect-error ldiff.error.1 (let ((x 10))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.2 (let ((x 'a))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.3 (let ((x (make-array '(10) :initial-element 'a)))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.4 (let ((x 1.23 ))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.5 (let ((x #\w))(ldiff x 'a)) :type type-error)

(test-expect-error map-1 (MAPCON #'APPEND) :type program-error)
(test-expect-error map-2 (MAPLIST #'APPEND) :type program-error)
(test-expect-error map-3 (MAPL #'APPEND) :type program-error)
(test-expect-error map-4 (MAPCAN #'APPEND) :type program-error)
(test-expect-error map-5 (MAPCAR #'APPEND) :type program-error)
(test-expect-error map-6 (MAPC #'APPEND) :type program-error)

(test last.8 (eq 'b (last '(a . b) 0)))
;;; 14.2.29 last
(test last-ansi-1 (equal '(c)  (last '(a b c))))
(test last-ansi-2 (null (last '(a b c) 0)))
(test last-ansi-3 (equal '(c) (last '(a b c) 1)))
(test last-ansi-4 (equal '(b c) (last '(a b c) 2)))
(test last-ansi-5 (equal '(a b c) (last '(a b c) 3)))
(test last-ansi-6 (equal '(a b c) (last '(a b c) 4)))
(test last-ansi-7 (equal (cons 'a 'b) (last '(a . b) 1)))
(test last-ansi-8 (equal (cons 'a 'b) (last '(a . b) 2)))
(test ACONS.3 (acons :a :b :c))

(test APPEND.7
      (not
      (LET ((X (LIST 'A 'B 'C 'D)))
        (EQ (APPEND X NIL) X))))
