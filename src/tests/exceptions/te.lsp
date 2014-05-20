
(let ((result (catch 'a
		(bformat t "1. In catch forms\n")
		(throw 'a 'b)
		(bformat t "2. Last catch form\n")
		'c)))
  (bformat t "3. Leaving let result = %s\n" result))



#|
(defun test-unwind-protect (v)
    (tagbody 
       (block nil
	 (unwind-protect
	      (progn
		(bformat t "1. Starting outer protected form\n")
		(unwind-protect
		     (progn
		       (bformat t "2. In inner protected form\n")
		       (when v (return-from nil 'done)))
		  (bformat t "3. In inner unwind form\n"))
		(bformat t "4. Finishing outer protected form\n"))
	   (progn
	     (bformat t "5. Outer unwind form\n")
	     (unwind-protect
		  (bformat t "6. In outer unwind protect form\n")
	       (bformat t "7. In outer unwind unwind form\n"))
	     ))
	 (bformat t "8. Leaving block\n"))
     done
       (bformat t "9 Leaving tagbody\n")))

(bformat t "With return-from --> result: 1. 2. 3. 5. 6. 7. 9.\n")
(test-unwind-protect t)
(bformat t "No return-from  --> result: 1. 2. 3. 4. 5. 6. 7. 8. 9.\n")
(test-unwind-protect nil)
|#



#|

(defun a (x) (bformat t "In a x=%s\n" x) x)


|#


#|
(let ((a 1)
      (*c* nil)
      (d 4))
  (declare (special *c*))
  (bformat t "a[%s] *c*[%s] d[%s]\n" a *c* d))
|#



#|
(locally (declare (special *uu*))
  (setq *uu* 10)
  (let ((a *uu*))
    (bformat t "a(*uu*) = %d\n" a)))
|#

#|
(the fixnum (+ 1 5))
|#
#|
(let (a b *c*)
  (setq a 1)
  (setq b 2)
  (setq *c* 3)
  (list a b *c*))

|#
#|
(defmacro when (pred &rest body)
  "Syntax: (when test {form}*)
If TEST evaluates to T, then evaluates FORMs and returns all values of the last
FORM. If not, simply return NIL."
  `(IF ,pred (progn ,@body) ()))
|#



#|
(defun zzz (u)
  (labels ((a () (bformat t "In a u[%s]\n" u))
	   (b () (bformat t "In b u[%s]\n" u)))
    (a)
    (b))
)

(zzz 10)
|#



#|
(progn
  (print 1234)
  (print 4567))

|#
#|
(quote ( a b c) )
|#

#|
(let ((x 2))
  (print (+ 1 x 3)))
|#


#|
(defparameter *a* 1)
(defparameter *b* 2)
(defparameter *c* 3)
(print (list *a* 111 *b* 222 *c* 333))
|#


#|
(let* ((x 1)
       (y (+ 99 x)))
  (bformat t "y=%d\n" y))
|#

#|
(if (= 1 0) (bformat t "It is TRUE\n") (bformat t "It is false\n"))
|#


#|
(let ((x 2))
  (bformat t "x = %d\n" x)
  (if (= x 1)
      (bformat t "(= x[%d] 1) is True" x)
      (bformat t "(/= x[%d] 1) is True" x)))
|#



#|
			(if (/= 1 1)
			    (print "1 /= 1!")
			    (print "Its true 1 == 1"))))

|#

#|
(compiler:compile-run "Hello there")
|#

#|
(test (= (compiler:compile-run 5) 5))
|#

#|
(print 23842093725072340572039482093752095342097350276)
|#


#|
(print 1234.5678)
|#












#|
(funcall (let ((a 1)(y 9999)) (function (lambda (x) (bformat t "Evaluating compiled function x=%s y=%s\n" x y)))) 98765)
|#

#|
(funcall (let ((a 1)(y 9999))
	   (function (lambda (x &optional (z "yadda") &key (u 777 ugot) &aux (j 567))
	     (bformat t "Evaluating compiled function x=%s y=%s z=%s u=%s ugot=%s j=%s\n" x y z u ugot j))))
	 98765 "hi there" )
|#


#|
(compiler:compile-run '(funcall (let ((a 1)(y 9999))
				  (function (lambda (x &optional (z "yadda") &key (u 777 ugot) &aux (j 567))
				    (bformat t "Evaluating compiled function x=%s y=%s z=%s u=%s ugot=%s j=%s\n" x y z u ugot j))))
			98765 "hi there" :u 98483848 ))

(compiler:compile-run '(funcall (let ((a 1)(y 9999))
				  (function (lambda (x &optional (z "yadda") &key (u 777 ugot) &allow-other-keys &aux (j 567))
				    (bformat t "Evaluating compiled function x=%s y=%s z=%s u=%s ugot=%s j=%s\n" x y z u ugot j))))
			98765 "hi there" :u 98483848 :v 7777))
(funcall (let ((a 1)(y 9999))
	   (function (lambda (x &optional (z "yadda") &key (u 777 ugot) &aux (j 567))
	     (bformat t "Evaluating compiled function x=%s y=%s z=%s u=%s ugot=%s j=%s\n" x y z u ugot j))))
	 98765 "hi there" :u 98483848 :v "tada" :allow-other-keys t)

|#


#|
(tagbody
 tag1
   (let ((x 10))
     (let ((y 20))
       (let ((z 30))
	 (print "At tag1")
	 (go tag3))))
 tag2
   (print "At tag2")
   (print "Another tag2")
 tag3
   (print "At tag3")
   )
|#

#|
(let ((x 10))
  (tagbody
     top
     (bformat t "x=%d\n" x)
     (setq x (- x 1))
     (if (= x 0)
	 (go done)
	 (go top)
	 )
     done
     (bformat t "Done\n")
     ))
|#

#|
(defun ttt (z)
  (block nil
    (let ((x 999))
      (block ablock
	(if 1
	    (progn
	      (bformat t "About to skip rest x = %d z = %s\n" x z)
	      (return-from ablock))
	    (bformat t "not one\n"))))
    (bformat t "last block\n")))

(ttt 1)
(ttt "Last one")
|#
  


#|
(co:compile-run '(let ((a (list 1 2 3 4 5)))
		   (dolist (i a)
		     (print i))))

|#


#|
(co:compile-run '(let ((j 10))
		  (setq j (if (= j 1)
			      (/ j 2)
			      (+ (* j 3) 1)))
		  (print j)
		  ))

|#

#|
(co:compile-run '(let ((a (list 1 2 3 4 5)))
		  ( BLOCK nil
		    ( LET* (( %DOLIST-VAR A )
			    I )
		      ( DECLARE )
		      ( WHILE %DOLIST-VAR
			( SETQ I ( FIRST %DOLIST-VAR ) )
			( PRINT I )
			( SETQ %DOLIST-VAR ( CDR %DOLIST-VAR ) ) )
		      nil ) ) ))
|#

#|
(co:compile-run '(let ((a 1))
		  (print a)))
|#
