(declaim (optimize (debug 3)))

;;
;; Lexical environments 

;; From CLHS
;; 3.1.1.3 Lexical Environments
;;
;; A lexical environment for evaluation at some position in a program
;; is that part of the environment that contains information having 
;; lexical scope within the forms containing that position. 
;; A lexical environment contains, among other things, the following:
;;
;; - bindings of lexical variables and symbol macros.
;; - bindings of functions and macros. (Implicit in this is information about those compiler macros that are locally disabled.)
;; - bindings of block tags.
;; - bindings of go tags.
;; - information about declarations.
;; 
;; The lexical environment that is active at any given position in a program 
;; being semantically processed is referred to by definite reference 
;; as ``the current lexical environment,'' or sometimes as just ``the lexical environment.''
;;
;; Within a given namespace, a name is said to be bound in a lexical environment 
;; if there is a binding associated with its name in the lexical environment or, 
;; if not, there is a binding associated with its name in the global environment.




;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of local environments, they contain the values of
;;; the local variables (but global and predefined variables).
;;; Runtime environment or, activation frames, are represented by 
;;; vectors (named v*). They have the following structure:
;;;           +------------+
;;;           | next       |  ---> next V*
;;;           | argument 0 |  value of the first argument
;;;           | argument 1 |  value of the second argument
;;;           .            .
;;;           | free slot  |  Free slot for nary variable
;;;           +------------+
;;; The number of arguments can be extracted from the size of the
;;; activation frame.

;;; A direct implementation with inlined vectors is approximatively 
;;; 7 times faster under sci.

(defstruct environment next)
(defstruct (activation-frame (:include environment)) argument)


(defun sr-extend* (sr v*)
  (setf (activation-frame-next v*) sr)
  v* )


(defun allocate-activation-frame (size)
  (make-activation-frame :argument (make-array (list size) :initial-element 'empty-arg )))


;;; Fetch the value of the Ith argument of the Jth frame.

(defun deep-fetch (sr i j)
  (if (= i 0)
      (elt (activation-frame-argument sr) j)
      (deep-fetch (environment-next sr) (- i 1) j) ) )

(defun deep-update! (sr i j v)
  (if (= i 0)
      (setf (elt (activation-frame-argument sr) j) v)
      (deep-update! (environment-next sr) (- i 1) j v) ) )



;; R is the static representation of the runtime local environment.
;; It is represented by a list of different kinds environments.
;; The different environments contain names that represent bindings:
;; variables-env    - bindings of lexical variables
;; symbol-macro-env - bindings of symbol macros.
;; function-env     - bindings of functions and macros. (Implicit in this is information about those compiler macros that are locally disabled.)
;; block-env        - bindings of block tags.
;; tags-env         - bindings of go tags.


(defstruct env next )
(defstruct (variables-env (:include env))  names )
(defstruct (symbol-macro-env (:include env)) names)
(defstruct (functions-env (:include env))  names )
(defstruct (block-env (:include env)) block-symbol )
(defstruct (tagbody-env (:include env)) tags)



;; Return an env that extends ENV by NAMES* 
(defun env-extend (env names*)
  (setf (env-next names*) env)
  names*)


(defun variables-env-lookup (env n)
  (when (variables-env-p env)
    (labels ((scan (names j)
	       (if names
		   (if (eq n (car names))
		       j
		       (scan (cdr names) (+ 1 j)))
		   nil)))
      (scan (variables-env-names env) 0))))


(defun functions-env-lookup (env n)
  (when (functions-env-p env)
    (labels ((scan (names j)
	       (if names
		   (if (eq n (car names))
		       j
		       (scan (cdr names) (+ 1 j)))
		   nil)))
      (scan (functions-env-names env) 0))))


(defun tagbody-env-lookup (env n)
  (when (tagbody-env-p env)
    (labels ((scan (tags j)
	       (if tags
		   (if (eq n (car tags))
		       j
		       (scan (cdr tags) (+ 1 j)))
		   nil)))
      (scan (tagbody-env-tags env) 0))))



;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable name.
;;; Two different answers.
;;; Or the variable is:
;;; global (ie created by the user) then return (GLOBAL . symbol)
;;; local (ie appears in R) then return     (LOCAL index . depth)

(defun variable-kind (env n)
  (or (global-variable? n)
      (local-variable? env 0 n)))


(defun local-variable? (env i n)
  (if env
      (let ((idx (variables-env-lookup env n)))
	(if idx
	    `(local ,i . ,idx)
	    (local-variable? (env-next env) (+ 1 i) n)))
      (error "Could not find lexical variable: ~a" n)))



#+cando
(defun global-variable? (n)
  (when (special-p n) `(global . ,n)))

#+sbcl
(defun global-variable? (n)
  (if (eq n 't)
      `(global . t)
      (when (eq (sb-cltl2:variable-information n) :special)
	`(global . ,n))))


;;
;; --------------------------------------------------
;;
;; Determine if a function is global or local
;;
(defun function-kind (env n)
  (or (local-function? env 0 n)
      (global-function? n)))


(defun local-function? (env i n)
  (if env
      (let ((idx (functions-env-lookup env n)))
	(if idx
	    `(local ,i . ,idx)
	    (local-function? (env-next env) (+ 1 i) n)))
      nil))

(defun global-function? (n)
  (when (fboundp n)
    `(global ,n)))



;;
;; Find a block symbol
;;

(defun block-symbol? (env n)
  (or (local-block? env 0 n)
      (error "Could not find block symbol: ~a" n)))

(defun local-block-symbol? (env i n)
  (if env
      (if (block-env-p env)
	  (if (eq (block-env-block-symbol env) n)
	      `(block . ,i)
	      (local-block-symbol? (env-next env) (+ 1 i) n))
	  (local-block-symbol? (env-next env) (+ 1 i) n))))


;;
;; Find a tagbody tag - return 

(defun tagbody-symbol? (env n)
  (or (local-tag? env 0 n)
      (error "Could not find tag: ~a" n )))


(defun local-tag? (env i n)
  (if env
      (if (tagbody-env-p env)
	  (let ((idx (tagbody-env-lookup env n)))
	    (if idx
		`(tag ,i . ,idx)
		(local-tag? (tagbody-env-next env) (+ 1 i) n)))
	  (local-tag? (env-next env) (+ 1 i) n))
      nil))




#|

(defun uuuu () (print "in uuuu"))

(defparameter *v* ())
(setq *v* (env-extend *v* (make-variables-env :names '(a b c))))
(setq *v* (env-extend *v* (make-block-env :block-symbol 'test)))
(setq *v* (env-extend *v* (make-functions-env :names '(rna rnd))))
(setq *v* (env-extend *v* (make-tagbody-env :tags '(ta tb tc td))))
(setq *v* (env-extend *v* (make-variables-env :names '(x y z))))

(local-variable? *v* 0 'c)


(function-kind *v* 'uuuu)
(function-kind *v* 'rna)

(meaning-function 'rna *v*)
(local-function? *v* 0 'rnd)

(block-symbol? *v* 0 'test)

(tagbody-symbol? *v* 'td)

|#




;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.



(defun meaning (e r)
  (declare (optimize (debug 3)))
  (if (atom e)
      (if (symbolp e)
	  (cond
	    ((not e) (meaning-quotation e r))
	    ((eq e t) (meaning-quotation e r))
	    (t (meaning-reference e r)))
	  (meaning-quotation e r) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r))
        ((lambda) (meaning-lambda (cadr e) (cddr e) r))
        ((if)     (meaning-alternative 
                   (cadr e) (caddr e) (cadddr e) r ))
        ((progn)  (meaning-sequence (cdr e) r))
;;        ((setq)   (meaning-assignment (cadr e) (caddr e) r))
        (t        (meaning-application (car e) (cdr e) r)) ) ) )



(defun meaning-alternative (e1 e2 e3 r)
  (let ((m1 (meaning e1 r))
	(m2 (meaning e2 r))
	(m3 (meaning e3 r)))
    (lambda (sr k)
      (funcall m1 sr (lambda (v)
		       (if v
			   (funcall m2 sr k)
			   (funcall m3 sr k)))))))

#|
(defparameter *r* (make-variables-env :names '(a b)))
*r*
(defparameter *a* (meaning '(if 1 (if nil 10 20) 2) nil))
(function-lambda-expression *a*)
(funcall *a* nil #'eval)

(funcall (meaning 't *r*) nil #'eval)
(funcall (meaning '() *r*) nil #'eval)
(atom nil)
(atom '())
(atom ())
(atom t)
(atom 't)
(symbolp nil)
(symbolp '())
(symbolp ())
(symbolp t)
(symbolp 't)
|#


;;; variant: if incrementally compiling we can check if the global
;;; variable is already initialized or not. But here we cannot
;;; backpatch the produced code to take benefit of the initialization
;;; of the variable.
(defun meaning-reference (n r)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (sr k)
                   (funcall k (elt (activation-frame-argument sr) j) ))
                 (lambda (sr k)
                   (funcall k (deep-fetch sr i j)) ) ) ))
          ((global)
           (let ((i (cdr kind)))
	     (lambda (sr k)
	       (declare (ignore sr))
	       (funcall k (symbol-value i))))))
        (error "No such variable: ~a" n) ) ) )

(defun meaning-quotation (v r)
  (declare (ignore r))
  (lambda (sr k)
    (declare (ignore sr))
    (funcall k v ) ))





;;; Application meaning.

(defun meaning-application (e e* r)
  (declare (optimize (debug 3)))
  #|  (cond 
  ((and (symbolp e)
  (let ((kind (function-kind r e)))
  (or (and (consp kind)
  (eq 'global (car kind))
  (meaning-global-function (cdr kind) e* r))
  (and (consp kind)
  (eq 'local (car kind))
  (meaning-local-function ( 
  (let ((desc (get-description e)))
  (and desc
  (eq 'function (car desc))
  (if (= (length (cddr desc)) (length e*))
  (meaning-primitive-application e e* r)
  (error "Incorrect arity for ~a" e) ) )
  ) ) ) ))
  ((and (consp e)
  (eq 'lambda (car e)) )
  (meaning-closed-application e e* r) )
  (t 
  |#
  (meaning-regular-application e e* r)) 

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.
#|
(defun meaning-closed-application (e ee* r)
  (let ((nn* (cadr e)))
    (labels ((parse (n* e* regular)))
      (cond ((consp n*) 
	     (if (consp e*)
		 (parse (cdr n*) (cdr e*) (cons (car n*) regular))
		 (error "Too less arguments ~a ~a" e ee*) ) )
	    ((null n*)
	     (if (null e*)
		 (meaning-fix-closed-application 
		  nn* (cddr e) ee* r )
		 (error "Too much arguments ~a ~a" e ee*) ) )
	    (t (meaning-dotted-closed-application 
		(reverse regular) n* (cddr e) ee* r )) )
      (parse (nn* ee* '())))
    ))

(defun meaning-fix-closed-application (n* body e* r)
  (let* ((m* (meaning* e* r (length e*)))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2)) )
    (lambda (sr k)
      (funcall m* sr (lambda (v*)
		       (funcall m+ (sr-extend* sr v*) k) )) ) ) )

(defun meaning-dotted-closed-application (n* n body e* r)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*)))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2)) )
    (lambda (sr k)
      (funcall m* sr (lambda (v*)
		       (funcall m+ (sr-extend* sr v*) k) )) ) ) )
|#
;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
#|
(defun meaning-primitive-application (e e* r)
  (let* ((desc (get-description e)) ; desc = (function address . variables-list)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (lambda (sr k) (funcall k (address))))
      ((1) 
       (let ((m1 (meaning (car e*) r)))
         (lambda (sr k) 
           (funcall m1 sr (lambda (v) 
                    (funcall k (address v)) )) ) ) )
      ((2) 
       (let ((m1 (meaning (car e*) r))
             (m2 (meaning (cadr e*) r)) )
         (lambda (sr k) 
           (funcall m1 sr (lambda (v1)
                    (funcall m2 sr (lambda (v2)
                             (funcall k (address v1 v2)) )) )) ) ) )
      ((3) 
       (let ((m1 (meaning (car e*) r))
             (m2 (meaning (cadr e*) r))
             (m3 (meaning (caddr e*) r)) )
         (lambda (sr k) 
           (funcall m1 sr (lambda (v1)
			    (funcall m2 sr (lambda (v2)
					     (funcall m3 sr (lambda (v3)
							      (funcall k (address v1 v2 v3))
							      )) )) )) ) ) )
      (t (meaning-regular-application e e* r)) ) ) )
|#

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(defun meaning-regular-application (e e* r)
  (declare (optimize (debug 2)))
  (let* ((m (meaning-function e r))
         (m* (meaning* e* r (length e*))) )
    (lambda (sr k)
      (declare (optimize (debug 3)))
      (funcall m sr
	       (lambda (f)
		 (if (functionp f)
		     (funcall m* sr
			      (lambda (v*) ;; Here we should call with the activation frame
				(let ((arglist (coerce (activation-frame-argument v*) 'list)))
				  (funcall k (apply f arglist)))
				  ))
		     (error "Not a function: ~a" f) ) )) ) ) )


(defun meaning-function (e r)
  (let ((kind (function-kind r e)))
    (cond
      ((eq 'global (car kind))
       (lambda (sr k) ; global functions don't need sr
	 (declare (optimize (debug 3)) (ignore sr))
	 (funcall k (symbol-function e))))
      ((eq 'local (car kind))
       (lambda (sr k)
	 (declare (optimize (debug 3)))
	 (funcall k sr (deep-fetch sr (cadr kind) (cddr kind)))))
      (t (error "Could not find function ~a" e)))))
  

(defun meaning* (e* r size)
  (if (consp e*)
      (meaning-some-arguments (car e*) (cdr e*) r size)
      (meaning-no-argument r size) ) )

;;; The activation frame has as much slots as there are terms in the
;;; application but this may be insufficient for instance if (f a b)
;;; is the appplication and f is (lambda (x y . z) ...) in that case,
;;; the activation frame requires an extra slot to hold () for Z.

(defun meaning-some-arguments (e e* r size)
  (let ((m (meaning e r))
        (m* (meaning* e* r size))
        (rank (- size (+ (length e*) 1))) )
    (lambda (sr k)
      (funcall m sr
	       (lambda (v)
		 (funcall m* sr
			  (lambda (v*)
			    (setf (elt (activation-frame-argument v*) rank) v)
			    (funcall k v*) )) )) ) ) )


(defun meaning-no-argument (r size)
  (declare (ignore r))
    (lambda (sr k)
      (declare (ignore sr))
      (let ((v* (allocate-activation-frame size)))
        (funcall k v*) ) ) ) 




;;
;; This should take declares shouldn't it?
;;
(defun meaning-sequence (e+ r)
  (if (consp e+)
      (if (consp (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r)
          (meaning*-single-sequence (car e+) r) )
      (error "Illegal syntax: (progn)") ) )

(defun meaning*-single-sequence (e r) 
  (meaning e r) )

(defun meaning*-multiple-sequence (e e+ r)
  (let ((m1 (meaning e r))
        (m+ (meaning-sequence e+ r)) )
    (lambda (sr k)
      (funcall m1 sr (lambda (v)
		       (declare (ignore v))
		       (funcall m+ sr k) )) ) ) )





#|
;;
;; STILL WORKING ON THIS
;;
;;
(defun meaning-lambda (nn* e+ r)
  (multiple-value-bind (declares docstring body)
      (process-declarations e+)
    (let* ((llh (make-lambda-list-handler nn* declares))
	   (all-symbols (all-arguments-list llh))
	   (r2 (env-extend r (make-variables-env :names all-symbols)))
	   (m+ (meaning-sequence body r2)) )
      (lambda (sr k)
	(funcall k
		 (lambda (v* k1)
		   (if (= (length (activation-frame-argument v*)) arity+1)
		       (funcall m+ (sr-extend* sr v*) k1)
		       (error "Incorrect arity") ) )) ) ) )

      
      (
			(
  (let ((llh (make-lambda-list-handler 
  (labels ((parse (n* regular)
	     (cond
	       ((consp n*) (parse (cdr n*) (cons (car n*) regular)))
	       ((null n*) (meaning-fix-abstraction nn* e+ r))
	       (t (meaning-dotted-abstraction 
		   (reverse regular) n* e+ r )) ) ) )
    (parse nn* '())))


|#

(defun meaning-fix-abstraction (n* e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ arity 1))
         (r2 (env-extend r (make-variables-env :names n*))) ;; (r-extend* r n*))  ;; (setq *v* (env-extend *v* (make-variables-env :names '(a b c))))
         (m+ (meaning-sequence e+ r2)) )
    (lambda (sr k)
      (funcall k
	       (lambda (v* k1)
		 (if (= (length (activation-frame-argument v*)) arity+1)
		     (funcall m+ (sr-extend* sr v*) k1)
		     (error "Incorrect arity") ) )) ) ) )

(defun meaning-dotted-abstraction (n* n e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ arity 1))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2)) )
    (lambda (sr k)
      (funcall k
	       (lambda (v* k1)
		 (if (>= (activation-frame-argument-length v*) arity+1)
		     (progn
		       (listify! v* arity)
		       (funcall m+ (sr-extend* sr v*) k1) )
		     (error "Incorrect arity") ) )) ) ) )




#|

(trace meaning-application
       meaning-regular-application
       meaning*
       )
(defparameter *a* (meaning '(progn
			     (print "Hello")
			     (print "there")
			     (elt '(1 2 3) 2)) nil )
			   )


*a*


(funcall *a* nil (lambda (x) (format t "Result = ~a~%" x)))

(apply #'elt '((1 2 3) 2))
  |#

