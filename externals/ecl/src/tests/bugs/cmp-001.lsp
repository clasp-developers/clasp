;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)

;;; Date: 12/03/2006
;;; From: Dan Corkill
;;; Fixed: 14/04/2006 (juanjo)
;;; Description:
;;;
;;;	The inner RETURN form should return to the outer block.
;;;     However, the closure (lambda (x) ...) is improperly translated
;;;	by the compiler to (lambda (x) (block nil ...) and thus this
;;;	form outputs '(1 2 3 4).
;;;
(deftest cmp-0001-block
    (funcall (compile nil
		      '(lambda ()
			(block nil
			  (funcall 'mapcar
				   #'(lambda (x)
				       (when x (return x)))
				   '(1 2 3 4))))
		      ))
  1)

;;; Fixed: 12/01/2006 (juanjo)
;;; Description:
;;;
;;;	COMPILE-FILE-PATHNAME now accepts both :FAS and :FASL as
;;;	synonyms.
;;;
;;;
(deftest cmp-0002-pathname
    (and (equalp (compile-file-pathname "foo" :type :fas)
		 (compile-file-pathname "foo" :type :fasl))
	 t)
  t)

;;; Fixed: 21/12/2005 (juanjo)
;;; Description:
;;;
;;;	Compute the path of the intermediate files (*.c, *.h, etc)
;;;	relative to that of the fasl or object file.
;;;

(deftest cmp-0003-paths
    (let* ((output (compile-file-pathname "tmp/aux" :type :fasl))
	   (h-file (compile-file-pathname output :type :h))
	   (c-file (compile-file-pathname output :type :c))
	   (data-file (compile-file-pathname output :type :data)))
      (and 
       (zerop (si::system "rm -rf tmp; mkdir tmp"))
       (with-compiler ("aux-cmp-0003-paths.lsp" :output-file output :c-file t
		       :h-file t :data-file t)
	 '(defun foo (x) (1+ x)))
       (probe-file output)
       (probe-file c-file)
       (probe-file h-file)
       (probe-file data-file)
       (delete-file "aux-cmp-0003-paths.lsp")
       t))
    t)

;;; Date: 08/03/2006
;;; From: Dan Corkill
;;; Fixed: 09/03/2006 (juanjo)
;;; Description:
;;;
;;;	DEFCONSTANT does not declare the symbol as global and thus the
;;;	compiler issues warnings when the symbol is referenced in the
;;;	same file in which it is defined as constant.
;;;

#-ecl-bytecmp
(deftest cmp-0004-defconstant-warn
    (let ((warn nil))
      (with-dflet ((c::cmpwarn (setf warn t)))
	(with-compiler ("aux-cmp-0004.lsp")
	  '(defconstant foo (list 1 2 3))
	  '(print foo)))
      (delete-file "aux-cmp-0004.lsp")
      (delete-file (compile-file-pathname "aux-cmp-0004.lsp" :type :fas))
      warn)
  nil)

;;; Date: 16/04/2006
;;; From: Juanjo
;;; Fixed: 16/04/2006 (juanjo)
;;; Description:
;;;
;;;	Special declarations should only affect the variable bound and
;;;	not their initialization forms. That, even if the variables are
;;;	the arguments of a function.
;;;

(deftest cmp-0005-declaration
    (let ((form '(lambda (y)
		  (flet ((faa (&key (x y))
			   (declare (special y))
			   x))
		    (let ((y 4))
		      (declare (special y))
		      (faa))))))
      ;; We must test that both the intepreted and the compiled form
      ;; output the same value.
      (list (funcall (compile 'nil form) 3)
	    (funcall (coerce form 'function) 3)))
  (3 3))

;;; Date: 26/04/2006
;;; From: Michael Goffioul
;;; Fixed: ----
;;; Description:
;;;
;;;	Functions with more than 64 arguments have to be invoked using
;;;	the lisp stack.
;;;

(deftest cmp-0006-call-arguments-limit
    (let ((form '(lambda ()
		  (list (list
		   'a0 'b0 'c0 'd0 'e0 'f0 'g0 'h0 'i0
		   'j0 'k0 'l0 'm0 'n0 'o0 'p0 'q0
		   'r0 's0 't0 'u0 'v0 'w0 'x0 'y0 'z0
		   'a1 'b1 'c1 'd1 'e1 'f1 'g1 'h1 'i1
		   'j1 'k1 'l1 'm1 'n1 'o1 'p1 'q1
		   'r1 's1 't1 'u1 'v1 'w1 'x1 'y1 'z1
		   'a2 'b2 'c2 'd2 'e2 'f2 'g2 'h2 'i2
		   'j2 'k2 'l2 'm2 'n2 'o2 'p2 'q2
		   'r2 's2 't2 'u2 'v2 'w2 'x2 'y2 'z2
		   'a3 'b3 'c3 'd3 'e3 'f3 'g3 'h3 'i3
		   'j3 'k3 'l3 'm3 'n3 'o3 'p3 'q3
		   'r3 's3 't3 'u3 'v3 'w3 'x3 'y3 'z3
		   'a4 'b4 'c4 'd4 'e4 'f4 'g4 'h4 'i4
		   'j4 'k4 'l4 'm4 'n4 'o4 'p4 'q4
		   'r4 's4 't4 'u4 'v4 'w4 'x4 'y4 'z4
		   'a5 'b5 'c5 'd5 'e5 'f5 'g5 'h5 'i5
		   'j5 'k5 'l5 'm5 'n5 'o5 'p5 'q5
		   'r5 's5 't5 'u5 'v5 'w5 'x5 'y5 'z5
		   'a6 'b6 'c6 'd6 'e6 'f6 'g6 'h6 'i6
		   'j6 'k6 'l6 'm6 'n6 'o6 'p6 'q6
		   'r6 's6 't6 'u6 'v6 'w6 'x6 'y6 'z6)))))
      (equal (funcall (compile 'foo form))
	     (funcall (coerce form 'function))))
  t)

;;; Date: 16/05/2005
;;; Fixed: 18/05/2006 (juanjo)
;;; Description:
;;;
;;;	The detection of when a lisp constant has to be externalized using MAKE-LOAD-FORM
;;;	breaks down with some circular structures
;;;

(defclass cmp-007-class ()
  ((parent :accessor cmp-007-parent :initform nil)
   (children :initarg :children :accessor cmp-007-children :initform nil)))

(defmethod make-load-form ((x cmp-007-class) &optional environment)
  (declare (ignore environment))
  (values
    ;; creation form
    `(make-instance ',(class-of x) :children ',(slot-value x 'children))
    ;; initialization form
    `(setf (cmp-007-parent ',x) ',(slot-value x 'parent))
     ))

(deftest cmp-0007-circular-load-form
    (loop for object in
	 (let ((l (list 1 2 3)))
	   (list l
		 (subst 3 l l)
		 (make-instance 'cmp-007-class)
		 (subst (make-instance 'cmp-007-class) 3 l)))
       collect (clos::need-to-make-load-form-p object nil))
  (nil nil t t))

;;; Date: 18/05/2005
;;; Fixed: 17/05/2006 (Brian Spilsbury & juanjo)
;;; Description:
;;;
;;;	The compiler is not able to externalize constants that have no printed representation.
;;;	In that case MAKE-LOAD-FORM should be used.
;;;

(deftest cmp-0008-make-load-form
    (let ((output (compile-file-pathname "aux-cmp-0008.lsp" :type :fasl)))
      (with-open-file (s "aux-cmp-0008.lsp" :if-exists :supersede :if-does-not-exist :create :direction :output)
	(princ "
(eval-when (:compile-toplevel)
 (defvar s4 (make-instance 'cmp-007-class))
 (defvar s5 (make-instance 'cmp-007-class))
 (setf (cmp-007-parent s5) s4)
 (setf (cmp-007-children s4) (list s5)))

(defvar a '#.s5)
(defvar b '#.s4)
(defvar c '#.s5)
(defun foo ()
  (let ((*print-circle* t))
    (with-output-to-string (s) (princ '#1=(1 2 3 #.s4 #1#) s))))
" s))
      (compile-file "aux-cmp-0008.lsp")
      (load output)
      (prog1 (foo)
	(delete-file output)
	(delete-file "aux-cmp-0008.lsp")))
  "#1=(1 2 3 #<a CL-TEST::CMP-007-CLASS> #1#)")

;;; Date: 9/06/2006 (Pascal Costanza)
;;; Fixed: 13/06/2006 (juanjo)
;;; Description:
;;;
;;;	A MACROLET function creates a set of local macro definitions.
;;;	The forms that expand these macros are themselves affected by
;;;	enclosing MACROLET and SYMBOL-MACRO definitions:
;;;		(defun bar ()
;;;		 (macrolet ((x () 2))
;;;		  (macrolet ((m () (x)))
;;;		   (m))))
;;;		(compile 'bar)
;;;		(bar) => 2
;;;
(deftest cmp-0009-macrolet
    (list
     (progn
       (defun bar ()
	 (macrolet ((x () 2))
	   (macrolet ((m () (x)))
	     (m))))
       (compile 'bar)
       (bar))
     (progn
       (defun bar ()
	 (symbol-macrolet ((x 2))
	   (macrolet ((m () x))
	     (m))))
       (compile 'bar)
       (bar)))
  (2 2))

;;; Fixed: 13/06/2006 (juanjo)
;;; Description:
;;;
;;;	A MACROLET that references a local variable from the form in
;;;	which it appears can cause corruption in the interpreter. We
;;;	solve this by signalling errors whenever such reference
;;;	happens.
;;;
;;;	Additionally MACROLET forms should not see the other macro
;;;	definitions on the same form, much like FLET functions cannot
;;;	call their siblings.
;;;
(deftest cmp-0010-macrolet
  (flet ((eval-with-error (form)
	   (handler-case (eval form)
	     (error (c) 'error))))
    (makunbound 'cmp-0010-foo)
    (fmakunbound 'cmp-0010-foo)
    (let ((faa 1))
      (declare (special faa))
      (mapcar #'eval-with-error
	      '((let ((faa 2))
		 (macrolet ((m () faa))
		   (m)))
		(let ((faa 4))
		 (declare (special faa))
		 (macrolet ((m () faa))
		   (m)))
		(let ((faa 4))
		 (declare (special cmp-0010-foo))
		 (macrolet ((m () cmp-0010-foo))
		   (m)))
		(let ((faa 5))
		 (macrolet ((m () cmp-0010-foo))
		   (m)))
		(macrolet ((cmp-0010-foo () 6))
		  (macrolet ((m () (cmp-0010-foo)))
		    (m)))
		(macrolet ((f1 () 7)
			   (f2 () 8))
		  ;; M should not see the new definitions F1 and F2
		  (macrolet ((f1 () 9)
			     (f2 () 10)
			     (m () (list 'quote (list (f1) (f2)))))
		    (m)))
		(flet ((cmp-0010-foo () 1))
		  (macrolet ((m () (cmp-0010-foo)))
		    (m)))
		(labels ((cmp-0010-foo () 1))
		  (macrolet ((m () (cmp-0010-foo)))
		    (m)))))))
  (error 1 error error 6 (7 8) error error ))

;;; Date: 22/06/2006 (juanjo)
;;; Fixed: 29/06/2006 (juanjo)
;;; Description:
;;;
;;;	ECL only accepted functions with less than 65 required
;;;	arguments. Otherwise it refused to compile the function. The fix must
;;;	respect the limit in the number of arguments passed in the C stack and
;;;	use the lisp stack for the other required arguments.
;;;
#-ecl-bytecmp
(deftest cmp-0011-c-arguments-limit
    (mapcar #'(lambda (nargs)
		(let* ((arg-list (loop for i from 0 below nargs
				       collect (intern (format nil "arg~d" i))))
		       (data (loop for i from 0 below nargs collect i))
		       (lambda-form `(lambda ,arg-list
				      (and (equalp (list ,@arg-list) ',data)
				       ,nargs)))
		       (c:*compile-verbose* nil)
		       (c:*compile-print* nil)
		       (function (compile 'foo lambda-form)))
		  (list (apply function (subseq data 0 nargs))
			(handler-case (apply function (make-list (1+ nargs)))
			  (error (c) :error))
			(handler-case (apply function (make-list (1- nargs)))
			  (error (c) :error)))))
	    '(10 20 30 40 50 63 64 65 70))
  ((10 :ERROR :ERROR) (20 :ERROR :ERROR) (30 :ERROR :ERROR) (40 :ERROR :ERROR)
   (50 :ERROR :ERROR) (63 :ERROR :ERROR) (64 :ERROR :ERROR) (65 :ERROR :ERROR)
   (70 :ERROR :ERROR)))

(let* ((nargs 10)
       (arg-list (loop for i from 0 below nargs
		       collect (intern (format nil "arg~d" i))))
       (arguments (make-list nargs)))
  (apply (compile 'foo `(lambda ,arg-list
			 (length (list ,@arg-list))))
	 arguments))

;;; Date: 12/07/2008 (Josh Elsasser)
;;; Fixed: 02/08/2008 (Juanjo)
;;; Description:
;;;
;;;	ECL fails to properly compute the closure type of a function that
;;;	returns a lambda that calls the function itself.
;;;
(deftest cmp-0012-compute-closure
  (and (with-compiler ("aux-cmp-0003-paths.lsp" :load t)
	 (defun testfun (outer)
	   (labels ((testlabel (inner)
		      (if inner
			  (testfun-map
			   (lambda (x) (testlabel x))
			   inner))
		      (print outer)))
	     (testlabel outer))))
       t)
  t)

;;; Date: 02/09/2008 (Josh Elsasser)
;;; Fixed: 12/09/2008 (Josh Elsasser)
;;; Description:
;;;
;;;	FTYPE proclamations and declarations do not accept user defined
;;;	function types.
;;;
(deftest cmp-0013-ftype-user-type
    (progn
      (deftype cmp-0013-float-function () '(function (float) float))
      (deftype cmp-0013-float () 'float)
      (loop for (type . fails) in
	   '(((function (float) float) . nil)
	     (cons . t)
	     (cmp-0013-float-function . nil)
	     (cmp-0013-float . t))
	 always (let ((form1 `(proclaim '(ftype ,type foo)))
		      (form2 `(compile nil '(lambda ()
					     (declare (ftype ,type foo))
					     (foo)))))
		  (if fails
		      (and (signals-error (eval form1) error)
			   (signals-error (eval form2) error)
			   t)
		      (progn
			(eval form1)
			(eval form2)
			t)))))
  t)

;;; Date: 01/11/2008 (E. Marsden)
;;; Fixed: 02/11/2008 (Juanjo)
;;; Description:
;;;
;;;	When compiled COERCE with type INTEGER may cause double
;;;	evaluation of a form.
(deftest cmp-0014-coerce
    (funcall
     (compile 'foo '(lambda (x) (coerce (shiftf x 2) 'integer)))
     1)
  1)

;;; Date: 03/11/2008 (E. Marsden)
;;; Fixed: 08/11/2008 (Juanjo)
;;; Description:
;;;
;;;	TYPEP, with a real type, produces strange results.
;;;
(deftest cmp-0015-coerce
    (funcall
     (compile 'foo '(lambda (x) (typep (shiftf x 1) '(real 10 20))))
     5)
  NIL)

;;; Date: 20/07/2008 (Juanjo)
;;; Fixed: 20/07/2008 (Juanjo)
;;; Description:
;;;
;;;	In the new compiler, when compiling LET forms with special variables
;;;	the values of the variables are not saved to make the assignments
;;;	really parallel.
;;;
(deftest cmp-0016-let-with-specials
    (progn
      (defvar *stak-x*)
      (defvar *stak-y*)
      (defvar *stak-z*)
      (funcall
       (compile
        nil
        '(lambda (*stak-x* *stak-y* *stak-z*)
          (labels
            ((stak-aux ()
               (if (not (< (the fixnum *stak-y*) (the fixnum *stak-x*)))
                   *stak-z*
                   (let ((*stak-x* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-x*))))
                                         (*stak-y* *stak-y*)
                                         (*stak-z* *stak-z*))
                                     (stak-aux)))
                         (*stak-y* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-y*))))
                                         (*stak-y* *stak-z*)
                                         (*stak-z* *stak-x*))
                                     (stak-aux)))
                         (*stak-z* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-z*))))
                                         (*stak-y* *stak-x*)
                                         (*stak-z* *stak-y*))
                                     (stak-aux))))
                     (stak-aux)))))
            (stak-aux)))) 18 12 6))
  7)

;;; Date: 06/10/2009 (J. Pellegrini)
;;; Fixed: 06/10/2009 (Juanjo)
;;; Description:
;;;	Extended strings were not accepted as documentation by the interpreter.
;;;
(deftest cmp-0017-docstrings
  (handler-case
   (progn
     (eval `(defun foo () ,(make-array 10 :initial-element #\Space :element-type 'character) 2))
     (eval (funcall 'foo)))
   (serious-condition (c) nil))
  2)

;;; Date: 07/11/2009 (A. Hefner)
;;; Fixed: 07/11/2009 (A. Hefner + Juanjo)
;;; Description:
;;;	ECL ignores the IGNORABLE declaration
;;;
(deftest cmp-0018-ignorable
    (let ((c::*suppress-compiler-messages* t))
      (and
       ;; Issue a warning for unused variables
       (handler-case (and (compile nil '(lambda (x y) (print x))) nil)
         (warning (c) t))
       ;; Do not issue a warning for unused variables declared IGNORE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignore y))
                                         (print x))) t)
         (warning (c) nil))
       ;; Do not issue a warning for unused variables declared IGNORABLE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignorable y))
                                         (print x))) t)
         (warning (c) nil))
       ;; Do not issue a warning for used variables declared IGNORABLE
       (handler-case (and (compile nil '(lambda (x y) (declare (ignorable x y))
                                         (print x))) t)
         (warning (c) nil))))
  t)

;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;	When calling a bytecodes (SETF ...) function from a compiled function
;;;	an invalid memory access is produced. This is actually a consequence
;;;	of a mismatch between the position of the fields bytecodes.entry
;;;	and cfun.entry
;;;
(deftest cmp-0019-bytecodes-entry-position
    (let ((indices (funcall (compile nil
                                     '(lambda ()
                                       (ffi:c-inline () () list "
	union cl_lispunion x[0];
	cl_index bytecodes = (char*)(&(x->bytecodes.entry)) - (char*)x;
	cl_index bclosure  = (char*)(&(x->bclosure.entry)) - (char*)x;
	cl_index cfun      = (char*)(&(x->cfun.entry)) - (char*)x;
	cl_index cfunfixed = (char*)(&(x->cfunfixed.entry)) - (char*)x;
	cl_index cclosure  = (char*)(&(x->cclosure.entry)) - (char*)x;
	@(return) = cl_list(5, MAKE_FIXNUM(bytecodes),
                            MAKE_FIXNUM(bclosure),
                            MAKE_FIXNUM(cfun),
                            MAKE_FIXNUM(cfunfixed),
                            MAKE_FIXNUM(cclosure));" :one-liner nil))))))
      (and (apply #'= indices) t))
  t)

;;; Date: 07/02/2010 (W. Hebich)
;;; Fixed: 07/02/2010 (Juanjo)
;;; Description:
;;;	THE forms do not understand VALUES types
;;;		(the (values t) (funcall sym))
;;;
(deftest cmp-0020-the-and-values
  (handler-case (and (compile 'foo '(lambda () (the (values t) (faa))))
                     t)
    (warning (c) nil))
  t)


;;; Date: 28/03/2010 (M. Mondor)
;;; Fixed: 28/03/2010 (Juanjo)
;;; Description:
;;;     ECL does not compile type declarations of a symbol macro
;;;
(deftest cmp-0021-symbol-macro-declaration
    (handler-case (and (compile 'nil
                                '(lambda (x)
                                  (symbol-macrolet ((y x))
                                    (declare (fixnum y))
                                    (+ y x))))
                       nil)
      (warning (c) t))
  nil)

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed 24/04/2010 (Juanjo)
;;; Description:
;;;     New special form, WITH-BACKEND.
;;;
(deftest cmp-0022-with-backend
    (progn
      (defparameter *cmp-0022* nil)
      (defun cmp-0022a ()
        (ext:with-backend
            :bytecodes (setf *cmp-0022* :bytecodes)
            :c/c++ (setf *cmp-0022* :c/c++)))
      (list
       (progn (cmp-0022a) *cmp-0022*)
       (cmp-0022a)
       (progn (compile 'cmp-0022a) (cmp-0022a) *cmp-0022*)
       (cmp-0022a)))
  (:bytecodes :bytecodes :c/c++ :c/c++))
