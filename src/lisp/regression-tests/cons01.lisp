(in-package #:clasp-tests)

(test adjoin-1 (let () (adjoin 1 (list 3 1 2))) ((3 1 2)))
(test adjoin-2 (let () (adjoin 5 (list 3 1 2))) ((5 3 1 2)))
(test adjoin-3 (let ()
                 (adjoin (list 1 2) (list (list 1 3) (list 2 4)) :key #'first))
      (((1 3) (2 4))))
(test pushnew-1
      (let ((x (list 3 1 2)))
        (pushnew 1 x)
        x)
      ((3 1 2)))
(test pushnew-2
      (let ((x (list 3 1 2)))
        (pushnew 5 x)
        x)
      ((5 3 1 2)))
(test pushnew-3
      (let ((x (list (list 1 3)(list 2 4))))
        (pushnew (list 1 2) x :key #'first)
        x)
      (((1 3) (2 4))))

;;; whether z and y have their values seems to depend on the order key and test are executed
;;;after discussion on #clasp, it seems to be a requirement that evaluation is left to right 
(test ansi-PUSHNEW.14a
      (LET ((I 0) X Y Z (D '(B C)))
        (VALUES
         (PUSHNEW (PROGN (SETF X (INCF I)) 'A) D
                  :TEST (PROGN (SETF Z (INCF I)) #'EQL)
                  :KEY (PROGN (SETF Y (INCF I)) #'IDENTITY))
         D I X Y Z))
      ((A B C) (A B C) 3 1 3 2))

(test ansi-PUSHNEW.14b
      (LET ((I 0) X Y Z (D '(B C)))
        (VALUES
         (PUSHNEW (PROGN (SETF X (INCF I)) 'A) D
                  :KEY (PROGN (SETF Y (INCF I)) #'IDENTITY)
                  :TEST (PROGN (SETF Z (INCF I)) #'EQL))
         D I X Y Z))
      ((A B C) (A B C) 3 1 2 3))
         
(test-expect-error GET-PROPERTIES.ERROR.5
                   (GET-PROPERTIES '(A 1 B 2 C 3 . D) '(X Y)) :type type-error)

(test ldiff-1 (LDIFF (COPY-TREE '(A B C D E . F)) 'A) ((A B C D E . F)))
(test-expect-error ldiff.error.1 (let ((x 10)) (ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.2 (let ((x 'a)) (ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.3(let ((x (make-array '(10) :initial-element 'a)))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.4 (let ((x 1.23 ))(ldiff x 'a)) :type type-error)
(test-expect-error ldiff.error.5 (let ((x #\w))(ldiff x 'a)) :type type-error)

(test-expect-error map-1 (MAPCON #'APPEND) :type program-error)
(test-expect-error map-2 (MAPLIST #'APPEND) :type program-error)
(test-expect-error map-3 (MAPL #'APPEND) :type program-error)
(test-expect-error map-4 (MAPCAN #'APPEND) :type program-error)
(test-expect-error map-5 (MAPCAR #'APPEND) :type program-error)
(test-expect-error map-6 (MAPC #'APPEND) :type program-error)

(test last.8 (last '(a . b) 0) (b))
;;; 14.2.29 last
(test last-ansi-1 (last '(a b c)) ((c)))
(test last-ansi-2 (last '(a b c) 0) (nil))
(test last-ansi-3 (last '(a b c) 1) ((c)))
(test last-ansi-4 (last '(a b c) 2) ((b c)))
(test last-ansi-5 (last '(a b c) 3) ((a b c)))
(test last-ansi-6 (last '(a b c) 4) ((a b c)))
(test last-ansi-7 (last '(a . b) 1) ((a . b)))
(test last-ansi-8 (last '(a . b) 2) ((a . b)))
(test-true ACONS.3 (acons :a :b :c))

(test-true APPEND.7
           (LET ((X (LIST 'A 'B 'C 'D)))
             (not (EQ (APPEND X NIL) X))))

(test-expect-error  MEMBER-IF.ERROR.8 (MEMBER-IF #'IDENTITY 'A) :type type-error)
(test-expect-error  ASSOC-IF.ERROR.12 (ASSOC-IF #'NULL '((A . B) :BAD (C . D))) :type type-error)
(test-expect-error  ASSOC-IF-NOT.ERROR.12 (ASSOC-IF-NOT #'IDENTITY '((A . B) :BAD (C . D))) :type type-error)

(test-true remf-failure-cl-http (let ((place (list 9000 23)))(remf place 9000)))

(test-expect-error LIST-LENGTH-SYMBOL (LIST-LENGTH 'A) :type type-error)
(test-expect-error LIST-LENGTH.ERROR.1 (list-length '(1 . 2)) :type type-error)
