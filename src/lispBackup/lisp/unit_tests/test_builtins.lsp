

(test (equal
       (mapcar #'list '(1 2) '(3 4))
       '((1 3) (2 4))))


(test (equal
       (let ((f '+))
	 (apply f '(1 2))) 3))

(test (equal
       (let ((f #'-))
	 (apply f '(1 2))) -1))

(test (equal
       (apply #'max 3 5 '(2 7 3)) 7))

(test (equal
       (apply 'cons '((+ 2 3) 4)) '((+ 2 3) . 4)))

(test (equal (apply #'+ '()) 0))


#|
(defun foo (size &rest keys &key double &allow-other-keys)
  (let ((v (apply #'make-array size :allow-other-keys t keys)))
    (if double (concatenate (type-of v) v v) v)))
(test (equal
       (foo 4 :initial-contents '(a b c d) :double t)
       #(A B C D A B C D)))
|#







(test-equal (mapcar #'car '((1 a) (2 b) (3 c)))  '(1 2 3))

(test-equal (mapcar #'abs '(3 -4 2 -5 -6)) '(3 4 2 5 6))

(test-equal (mapcar #'cons '(a b c) '(1 2 3)) '((A . 1) (B . 2) (C . 3)))

(test-equal (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
	    '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

(test-equal (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
	    '((FOO A B C D) (FOO B C D) (FOO C D) (FOO D)))

(test-equal (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))
		     '(a b a c d b c)) '(0 0 1 0 1 1 1))


;An entry is 1 if the corresponding element of the input
;  list was the last instance of that element in the input list.

(defparameter dummy nil)
(test-equal
 (mapc #'(lambda (&rest x) (setq dummy (append dummy x))) '(1 2 3 4) '(a b c d e) '(x y z))
 '(1 2 3 4) )
(test-equal dummy '(1 A X 2 B Y 3 C Z))

(setq dummy nil)
(test-equal (mapl #'(lambda (x) (push x dummy)) '(1 2 3 4)) '(1 2 3 4))
(test-equal dummy '((4) (3 4) (2 3 4) (1 2 3 4)))

(test-equal (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
		    '(nil nil nil d e)
		    '(1 2 3 4 5 6)) '(D 4 E 5))

(test-equal (mapcan #'(lambda (x) (and (numberp x) (list x)))
		    '(a 1 b c 3 4 d 5)) '(1 3 4 5))
