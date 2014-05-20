;;; $Id: chap6a.scm,v 4.5 2006/11/13 12:12:11 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                      Threaded interpreter.




;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.
#|
(define g.current '())

(define sg.current (make-vector 100))

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current (cons (cons n `(global . ,level)) g.current))
    level ) )

(define (global-fetch i)
  (vector-ref sg.current i) )

(define (global-update! i v)
  (vector-set! sg.current i v) )

(define (g.current-initialize! name)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((global)
           (vector-set! sg.current (cdr kind) undefined-value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.current-extend! name)))
          (vector-set! sg.current index undefined-value) ) ) )
  name )

;;; This tag is used in the value cell of uninitialized variables.

(define undefined-value (cons 'undefined 'value))
|#



;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.
#|
(define g.init '())

(define sg.init (make-vector 100))

(define (predefined-fetch i)
  (vector-ref sg.init i) )

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init (cons (cons n `(predefined . ,level)) g.init))
    level ) )

;;; Add that value is associated to name in the predefined global environment.

(define (g.init-initialize! name value)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((predefined)
           (vector-set! sg.init (cdr kind) value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value) ) ) )
  name )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.

(define desc.init '())

(define (description-extend! name description)
  (set! desc.init (cons (cons name description) desc.init))
  name )

;;; Return the description or #f if absent.

(define (get-description name)
  (let ((p (assq name desc.init)))
    (and (pair? p) (cdr p)) ) )
|#        

(defun meaning-quotation (v r)
  (lambda (sr k)
    (k v) ) )

(defun meaning-alternative (e1 e2 e3 r)
  (let ((m1 (meaning e1 r))
        (m2 (meaning e2 r))
        (m3 (meaning e3 r)) )
    (lambda (sr k)
      (m1 sr (lambda (v)
               ((if v m2 m3) sr k) )) ) ) )

(defun meaning-assignment (n e r) 
  (let ((m (meaning e r))
        (kind (compute-kind r n)) )
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (sr k)
                   (m sr (lambda (v)
                           (k (setf (elt (activation-frame-argument sr) j v )) )) ))
                 (lambda (sr k)
                   (m sr (lambda (v)
                           (k (deep-update! sr i j v)) )) ) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (lambda (sr k)
               (m sr (lambda (v)
                       (k (set i v)) )) ) ) ) )
        (static-wrong "No such variable" n) ) ) )

(defun meaning-sequence (e+ r)
  (if (consp e+)
      (if (consp (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r)
          (meaning*-single-sequence (car e+) r) )
      (static-wrong "Illegal syntax: (begin)") ) )

(defun meaning*-single-sequence (e r) 
  (meaning e r) )

(defun meaning*-multiple-sequence (e e+ r)
  (let ((m1 (meaning e r))
        (m+ (meaning-sequence e+ r)) )
    (lambda (sr k)
      (m1 sr (lambda (v)
               (m+ sr k) )) ) ) )

(defun meaning-abstraction (nn* e+ r)
  (let parse ((n* nn*)
              (regular nil) )
    (cond
     ((consp n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null n*) (meaning-fix-abstraction nn* e+ r))
     (else (meaning-dotted-abstraction 
            (reverse regular) n* e+ r )) ) ) )

(defun meaning-fix-abstraction (n* e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ arity 1))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2)) )
    (lambda (sr k)
      (k (lambda (v* k1)
           (if (= (length (activation-frame-argument v*)) arity+1)
               (m+ (sr-extend* sr v*) k1)
               (wrong "Incorrect arity") ) )) ) ) )

(defun meaning-dotted-abstraction (n* n e+ r)
  (let* ((arity (length n*))
         (arity+1 (+ arity 1))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2)) )
    (lambda (sr k)
      (k (lambda (v* k1)
           (if (>= (activation-frame-argument-length v*) arity+1)
               (progn
		 (listify! v* arity)
		 (m+ (sr-extend* sr v*) k1) )
               (wrong "Incorrect arity") ) )) ) ) )

;;; Gather into a list all arguments from arity+1 to the end of the
;;; activation frame and store this list into the arity+1th slot.

(defun listify! (v* arity)
  (let loop ((index (- (length (activation-frame-argument v*)) 1))
             (result '()) )
    (if (= arity index)
        (set-activation-frame-argument! v* arity result)
        (loop (- index 1)
              (cons (activation-frame-argument v* (- index 1))
                    result ) ) ) ) )

;;; Application meaning.
;;
;; Need to be able to find function in local or global environment
;;
;; WORKING HERE
;;
(defun meaning-application (e e* r)
  (cond 
    ((and (symbolp e)
	  (let ((kind (compute-function-kind r e)))
	    (and (consp kind)
		 (eq 'global (car kind))
		 (let ((desc (get-description e)))
		   (and desc
			(eq 'function (car desc))
			(if (= (length (cddr desc)) (length e*))
			    (meaning-primitive-application e e* r)
			    (static-wrong "Incorrect arity for" e) ) )
		   ) ) ) ))
    ((and (consp e)
	  (eq 'lambda (car e)) )
     (meaning-closed-application e e* r) )
    (else (meaning-regular-application e e* r)) ) )

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(define (meaning-closed-application e ee* r)
  (let ((nn* (cadr e)))
    (let parse ((n* nn*)
                (e* ee*)
                (regular '()) )
      (cond ((pair? n*) 
             (if (pair? e*)
                 (parse (cdr n*) (cdr e*) (cons (car n*) regular))
                 (static-wrong "Too less arguments" e ee*) ) )
            ((null? n*)
             (if (null? e*)
                 (meaning-fix-closed-application 
                  nn* (cddr e) ee* r )
                 (static-wrong "Too much arguments" e ee*) ) )
            (else (meaning-dotted-closed-application 
                   (reverse regular) n* (cddr e) ee* r )) ) ) ) )

(define (meaning-fix-closed-application n* body e* r)
  (let* ((m* (meaning* e* r (length e*)))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2)) )
    (lambda (sr k)
      (m* sr (lambda (v*)
               (m+ (sr-extend* sr v*) k) )) ) ) )

(define (meaning-dotted-closed-application n* n body e* r)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*)))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2)) )
    (lambda (sr k)
      (m* sr (lambda (v*)
               (m+ (sr-extend* sr v*) k) )) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.

(define (meaning-primitive-application e e* r)
  (let* ((desc (get-description e)) ; desc = (function address . variables-list)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (lambda (sr k) (k (address))))
      ((1) 
       (let ((m1 (meaning (car e*) r)))
         (lambda (sr k) 
           (m1 sr (lambda (v) 
                    (k (address v)) )) ) ) )
      ((2) 
       (let ((m1 (meaning (car e*) r))
             (m2 (meaning (cadr e*) r)) )
         (lambda (sr k) 
           (m1 sr (lambda (v1)
                    (m2 sr (lambda (v2)
                             (k (address v1 v2)) )) )) ) ) )
      ((3) 
       (let ((m1 (meaning (car e*) r))
             (m2 (meaning (cadr e*) r))
             (m3 (meaning (caddr e*) r)) )
         (lambda (sr k) 
           (m1 sr (lambda (v1)
                    (m2 sr (lambda (v2)
                             (m3 sr (lambda (v3)
                                      (k (address v1 v2 v3))
                                      )) )) )) ) ) )
      (else (meaning-regular-application e e* r)) ) ) )

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(define (meaning-regular-application e e* r)
  (let* ((m (meaning e r))
         (m* (meaning* e* r (length e*))) )
    (lambda (sr k)
      (m sr (lambda (f)
              (if (procedure? f)
                  (m* sr (lambda (v*)
                           (f v* k) ))
                  (wrong "Not a function" f) ) )) ) ) )

(define (meaning* e* r size)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size)
      (meaning-no-argument r size) ) )

(define (meaning-dotted* e* r size arity)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*) r size arity)
      (meaning-no-dotted-argument r size arity) ) )

;;; The activation frame has as much slots as there are terms in the
;;; application but this may be insufficient for instance if (f a b)
;;; is the appplication and f is (lambda (x y . z) ...) in that case,
;;; the activation frame requires an extra slot to hold () for Z.

(define (meaning-some-arguments e e* r size)
  (let ((m (meaning e r))
        (m* (meaning* e* r size))
        (rank (- size (+ (length e*) 1))) )
    (lambda (sr k)
      (m sr (lambda (v)
              (m* sr (lambda (v*)
                       (set-activation-frame-argument! v* rank v)
                       (k v*) )) )) ) ) )

(define (meaning-some-dotted-arguments e e* r size arity)
  (let ((m (meaning e r))
        (m* (meaning-dotted* e* r size arity))
        (rank (- size (+ (length e*) 1))) )
    (if (< rank arity)
        (lambda (sr k)
          (m sr (lambda (v)
                  (m* sr (lambda (v*)
                           (set-activation-frame-argument! v* rank v)
                           (k v*) )) )) )
        (lambda (sr k)
          (m sr (lambda (v)
                  (m* sr (lambda (v*)
                           (set-activation-frame-argument! 
                            v* arity
                            (cons v (activation-frame-argument 
                                     v* arity )) )
                           (k v*) )) )) ) ) ) )

(define (meaning-no-argument r size)
  (let ((size+1 (+ size 1)))
    (lambda (sr k)
      (let ((v* (allocate-activation-frame size+1)))
        (k v*) ) ) ) )

(define (meaning-no-dotted-argument r size arity)
  (let ((arity+1 (+ arity 1)))
    (lambda (sr k)
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        (k v*) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (g.init-initialize! 'name value) ) ) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value 0) (defprimitive0 name value))
    ((defprimitive name value 1) (defprimitive1 name value))
    ((defprimitive name value 2) (defprimitive2 name value))
    ((defprimitive name value 3) (defprimitive3 name value)) ) )    

(define-syntax defprimitive0
  (syntax-rules ()
    ((defprimitive0 name value)
     (definitial name
       (letrec ((arity+1 (+ 0 1))
                (behavior 
                 (lambda (v* k)
                   (if (= arity+1 (activation-frame-argument-length v*))
                       (k (value))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value))
         behavior ) ) ) ) )
  
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda (v* k)
                   (if (= arity+1 (activation-frame-argument-length v*))
                       (k (value (activation-frame-argument v* 0)))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         behavior ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda (v* k)
                   (if (= arity+1 (activation-frame-argument-length v*))
                       (k (value (activation-frame-argument v* 0) 
                                 (activation-frame-argument v* 1) ))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         behavior ) ) ) ) )
  
(define-syntax defprimitive3
  (syntax-rules ()
    ((defprimitive3 name value)
     (definitial name
       (letrec ((arity+1 (+ 3 1))
                (behavior 
                 (lambda (v* k)
                   (if (= (activation-frame-argument-length v*)
                          arity+1 )
                       (k (value (activation-frame-argument v* 0)
                                 (activation-frame-argument v* 1)
                                 (activation-frame-argument v* 2) ))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend!                  ; \modified
          'name `(function ,value a b c))
         behavior ) ) ) ) )

;;; Define a location in the user global environment.

(define-syntax defvariable
  (syntax-rules ()
    ((defvariable name) (g.current-initialize! 'name)) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (lambda (v* k)
      (if (= arity+1 (activation-frame-argument-length v*))
          ((activation-frame-argument v* 0)
           (let ((frame (allocate-activation-frame (+ 1 1))))
             (set-activation-frame-argument! 
              frame 0
              (lambda (values kk)
                (if (= (activation-frame-argument-length values)
                       arity+1 )
                    (k (activation-frame-argument values 0))
                    (wrong "Incorrect arity" 'continuation) ) ) )
             frame )
           k )
        (wrong "Incorrect arity" 'call/cc) ) ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (lambda (v* k)
      (if (>= (activation-frame-argument-length v*) arity+1)
          (let ((proc (activation-frame-argument v* 0)))
            (if (procedure? proc)
                (let* ((args-number
                        (- (activation-frame-argument-length v*) 1) )
                       ;; fresh cells
                       (result (append (activation-frame-argument 
                                        v* (- args-number 1) )
                                       '() )) )
                  (do ((i (- args-number 2) (- i 1)))
                      ((= i 0))
                    (set! result (cons (activation-frame-argument v* i)
                                       result )) )
                  (let* ((size (+ (length result) 1))
                         (frame (allocate-activation-frame size)) )
                    (do ((i 0 (+ i 1))
                         (result result (cdr result)) )
                        ((null? result))
                      (set-activation-frame-argument! frame i (car result)) )
                    (proc frame k) )  )
            (wrong "Not applyable" proc) ) )
          (wrong "Incorrect arity" 'apply) ) ) ) )

(definitial list
  (lambda (v* k)
    (let ((args-number (- (activation-frame-argument-length v*) 1))
          (result '()) )
      (do ((i args-number (- i 1)))
          ((= i 0))
        (set! result (cons (activation-frame-argument v* (- i 1)) result)) ) 
      (k result) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Some free global locations:

(defvariable x)
(defvariable y)
(defvariable z)
(defvariable a)
(defvariable b)
(defvariable c)
(defvariable foo)
(defvariable bar)
(defvariable hux)
(defvariable fib)
(defvariable fact)
(defvariable visit)
(defvariable length)
(defvariable primes)

;;; Testing

(define (chapter61-interpreter)
  (define (compile e) (meaning e r.init))
  (define (run c) (c sr.init display))
  (define (toplevel)
    (run (compile (read)))
    (toplevel) )
  (toplevel) )

(define (scheme6a)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (lambda ()
       (print ((meaning (read) r.init)
               sr.init
               (lambda (v) v) ) ) ) ) ) )

(define (test-scheme6a file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (lambda ()
       (check ((meaning (read) r.init)
               sr.init
               (lambda (v) v) ) ) ) )
   equal? ) )

(set! static-wrong 
      (lambda (message . culprits)
        (display `(*static-error* ,message . ,culprits))(newline)
        (lambda (sr k)
          (apply wrong message culprits) ) ) )

;;; Pay attention to tail-rec in Scheme->C.

(define (bench6a factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e r.init)) )
    (let loop ((factor factor))
      (m sr.init
         (lambda (v) 
           (let ((duration (- (get-internal-run-time) start)))
             (when (<= factor 1)
               (display (list duration v))
               (newline) ) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )

;;; Add check-syntax

;;; share more (lambda (sr k)..) closures

;;; end of chap6a.scm
|#





;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . symbol)

(defun compute-kind (r n)
  (or (when (global-variable? n) `(global . n))
      (local-variable? r 0 n)))




(defun local-variable? (r i n)
  (and (consp r)
       (labels ((scan (names j)
		  (cond ((consp names) 
			 (if (eql n (car names))
			     `(local ,i . ,j)
			     (scan (cdr names) (+ 1 j)) ) )
			((null names)
			 (local-variable? (cdr r) (+ i 1) n) )
			((eql n names) `(local ,i . ,j)) ) ) )
	 (scan (car r) 0)
	 )))


#+cando
(defun global-variable? (n)
  (when (special-p n) `(global ,n)))

#+sbcl
(defun global-variable? (n)
  (when (eq (sb-cltl2:variable-information n) :special)
    `(global ,n)))

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

(defstruct environment next )

(defstruct ( activation-frame
	     (:include environment ))
  argument )


(defstruct (function-environment (:include environment))
  functions )



(defun sr-extend* (sr v*)
    (setf (environment-next v*) sr)
  v* )

(defparameter sr.init nil)


;;; Fetch the value of the Ith argument of the Jth frame.

(defun deep-fetch (sr i j)
  (if (= i 0)
      (elt (activation-frame-argument sr) j)
      (deep-fetch (environment-next sr) (- i 1) j) ) )

(defun deep-update! (sr i j v)
  (if (= i 0)
      (setf (elt (activation-frame-argument sr) j ) v)
      (deep-update! (environment-next sr) (- i 1) j v) ) )

;;; R is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(defun r-extend* (r n*)
  (cons n* r) )

(defparameter *r.init* nil)





;;; variant: if incrementally compiling we can check if the global
;;; variable is already initialized or not. But here we cannot
;;; backpatch the produced code to take benefit of the initialization
;;; of the variable.
(defun meaning-reference ( n r)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (sr k)
                   (k (elt (activation-frame-argument sr) j)) )
                 (lambda (sr k)
                   (k (deep-fetch sr i j)) ) ) ) )
          ((global)
           (let ((i (cdr kind)))
	     (lambda (sr k)
	       (k (symbol-value i))))))
	(error "No such variable ~a" n ))))



;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.

(defun meaning (e r)
  (if (atom e)
      (if (symbolp e) (meaning-reference e r)
                      (meaning-quotation e r) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r))
        ((if)     (meaning-alternative 
                   (cadr e) (caddr e) (cadddr e) r ))
        ((progn)  (meaning-sequence (cdr e) r))
        ((setq)   (meaning-assignment (cadr e) (caddr e) r))
        (else     (meaning-application (car e) (cdr e) r)) ) ) )

