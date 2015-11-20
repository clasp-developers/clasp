;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defun proper-list-p (object)
  (cond  ((null object) t)
	 ((atom object) nil)
	 (t (let ((slow object)
		  (fast (cdr object)))
	      (declare (type cons slow))
	      (tagbody
	       again
		 (unless (consp fast)
		   (return-from proper-list-p
		     (if (null fast) t nil)))
		 (when (eq fast slow)
		   (return-from proper-list-p nil))
		 (setq fast (cdr fast))
		 (unless (consp fast)
		   (return-from proper-list-p
		     (if (null fast) t nil)))
		 (setq fast (cdr fast))
		 (setq slow (cdr slow))
		 (go again))))))

(defun do+ (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          (car numbers)
          `(core:binary-+ ,(car numbers) ,(do+ (cdr numbers))))
      (if (null numbers)
          0
          (error "The + operator can not be part of a form that is a dotted list."))))

(define-compiler-macro + (&rest numbers)
  (if (null numbers)
      0
      (if (proper-list-p numbers)
          (do+ numbers)
          (error "The + operator requires a proper list not ~s" numbers))))

(defun do- (minuend subtrahends)
  (if (consp subtrahends)
      `(core:binary-- ,minuend ,(do+ subtrahends))
      (if (null subtrahends)
          `(core:negate ,minuend)
          (error "The - operator can not be part of a form that is a dotted list."))))

(define-compiler-macro - (minuend &rest subtrahends)
  (if (proper-list-p subtrahends)
      (do- minuend subtrahends)
      (error "The - operator requires a proper list")))

(defun do-< (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          t
          (if (= (length numbers) 2)
              `(core:binary-< ,(car numbers) ,(cadr numbers))
              `(if (core:binary-< ,(car numbers) ,(cadr numbers))
                   ,(do-< (cdr numbers))
                   nil)))))

(defun do-<= (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          t
          (if (= (length numbers) 2)
              `(core:binary-<= ,(car numbers) ,(cadr numbers))
              `(if (core:binary-<= ,(car numbers) ,(cadr numbers))
                   ,(do-<= (cdr numbers))
                   nil)))))

(defun do-> (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          t
          (if (= (length numbers) 2)
              `(core:binary-> ,(car numbers) ,(cadr numbers))
              `(if (core:binary-> ,(car numbers) ,(cadr numbers))
                   ,(do-> (cdr numbers))
                   nil)))))

(defun do->= (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          t
          (if (= (length numbers) 2)
              `(core:binary->= ,(car numbers) ,(cadr numbers))
              `(if (core:binary->= ,(car numbers) ,(cadr numbers))
                   ,(do->= (cdr numbers))
                   nil)))))

(defun do-= (numbers)
  (if (consp numbers)
      (if (= (length numbers) 1)
          t
          (if (= (length numbers) 2)
              `(core:binary-= ,(car numbers) ,(cadr numbers))
              `(if (core:binary-= ,(car numbers) ,(cadr numbers))
                   ,(do-= (cdr numbers))
                   nil)))))

(define-compiler-macro < (&rest numbers)
  (if (null numbers)
      (error "The < operator requires a list")
      (if (proper-list-p numbers)
          (do-< numbers)
          (error "The < operator requires a proper list not ~s" numbers))))

(define-compiler-macro <= (&rest numbers)
  (if (null numbers)
      (error "The <= operator requires a list")
      (if (proper-list-p numbers)
          (do-<= numbers)
          (error "The <= operator requires a proper list not ~s" numbers))))

(define-compiler-macro > (&rest numbers)
  (if (null numbers)
      (error "The > operator requires a list")
      (if (proper-list-p numbers)
          (do-> numbers)
          (error "The > operator requires a proper list not ~s" numbers))))

(define-compiler-macro >= (&rest numbers)
  (if (null numbers)
      (error "The >= operator requires a list")
      (if (proper-list-p numbers)
          (do->= numbers)
          (error "The >= operator requires a proper list not ~s" numbers))))

(define-compiler-macro = (&rest numbers)
  (if (null numbers)
      (error "The = operator requires a list")
      (if (proper-list-p numbers)
          (do-= numbers)
          (error "The = operator requires a proper list not ~s" numbers))))

(define-compiler-macro 1+ (x)
  `(core:binary-+ ,x 1))

(define-compiler-macro 1- (x)
  `(core:binary-- ,x 1))

(time (dotimes (i 1000000) (+ 3 4)))


(defun foo (x y) (+ x y))
(disassemble 'foo)
(macroexpand '(or 1 2 
