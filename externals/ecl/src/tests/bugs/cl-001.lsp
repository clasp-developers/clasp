;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)

;;; Date: 09/05/2006
;;; From: Brian Spilsbury
;;; Fixed: 20/05/2006 (Brian Spilsbury)
;;; Description:
;;;
;;;	(DEFPACKAGE "FOO" (:USE) (:IMPORT-FROM "CL" "NIL" "T"))
;;;	fails to import symbol NIL because IMPORT is invoked as
;;;	(IMPORT NIL (find-package "CL")), which does not import
;;;	any symbol.
;;;

(deftest cl-0001-import
   (progn
    (defpackage "FOO" (:USE) (:IMPORT-FROM "CL" "NIL" "T"))
    (prog1 (multiple-value-list (find-symbol "NIL" (find-package "FOO")))
     (delete-package "FOO")))
 (NIL :INTERNAL))

;;; Date: 09/05/2006
;;; From: Brian Spilsbury
;;; Fixed: 20/05/2006 (Brian Spilsbury)
;;; Description:
;;;
;;;	 Compiled FLET forms failed to shadow global macro definitions, if not
;;;	 for the compiler, at least for MACRO-FUNCTION and MACROEXPAND[-1]
;;;

(deftest cl-0002-macro-shadow
   (progn
     (with-compiler ("aux-cl-0002.lsp")
       '(defmacro foo () 2)
       '(defmacro test (symbol &environment env)
	  (and (macro-function symbol env) t))
       '(defun doit () (flet ((foo () 1)) (test foo))))
     (load "aux-cl-0002")
     (delete-file "aux-cl-0002.lsp")
     (delete-file (compile-file-pathname "aux-cl-0002" :type :fas))
     (prog1
	 (doit)
       (fmakunbound 'doit)
       (fmakunbound 'test)
       (fmakunbound 'foo)))
   NIL)

;;;
;;; Fixed: 14/06/2006 (juanjo)
;;; Description:
;;;
;;;	APROPOS, APROPOS-LIST and HELP* are case sensitive.
;;;

(deftest cl-0003-apropos
  (and (equal (apropos-list "bin")
	      (apropos-list "bin"))
       t)
  t)

;;; Date: 08/07/2006 (Dave Roberts)
;;; Fixed: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;	SLIME traps when invoking DESCRIBE. Reason is that STREAMP breaks on
;;;	Gray streams.
;;;

(deftest cl-0004-streamp
    (streamp (make-instance 'gray:fundamental-stream))
  t)

;;; Date: 02/08/2006 (juanjo)
;;; Description:
;;;
;;;	There is a problem with SUBTYPEP and type STREAM
;;;

(deftest cl-0005-subtypep-stream
    (subtypep (find-class 'gray:fundamental-stream) 'stream)
  t t)

;;; Date: 09/07/2006 (Tim S)
;;; Fixed: 09/07/2006 (Tim S)
;;; Description:
;;;
;;;	ENOUGH-NAMESTRING provided too large pathnames even when the
;;;	pathname was a subdirectory of the default pathname.
;;;
;;; Date: 31/12/2006 (Richard M. Kreuter)
;;; Fixed: 5/1/2007 (Juanjo)
;;; Description:
;;;	ENOUGH-NAMESTRING does not simplify the pathname when the
;;;	directory matches completely that of the default path.
;;;

(defvar *enough-namestring_tests*
 `(("/A/b/C/"
    ("/A/b/C/drink-up.sot"
     "/A/b/C/loozer/whiskey.sot"
     "/A/b/C/loozer/whiskey"
     "/A/b/whiskey.sot"
     "/A/"
     "whiskey.sot"
     "loozer/whiskey.sot"
     "C/loozer/whisky.sot"
     ""))
   ("A/b/C" ("A/b/C" "A/b/C/loozer" "b/C" "/A/b/C" "/A/" ""))
   ("/" ("/A/b/C/drink-up.sot" "/A/b/C/" "/A/" ""))
   ("" ("/A/b/C/drink-up.sot" "/A/b/C/loozer/whiskey.sot"
        "/A/b/C/loozer/whiskey" "/A/b/whiskey.sot"
        "/A/" "whiskey.sot" "loozer/whiskey.sot" "C/loozer/whisky.sot"))
   ("/A/*/C/drink-up.sot"
    ("/A/*/C/drink-up.sot" "/A/b/C/drink-up.sot" "/A/b/C/loozer/whiskey.*"
     "/A/b/C/loozer/*.sot" "/A/**/whiskey.sot" ""))
   ("/A/b/../c/d.sot" ("/A/b/../c/d.sot" "/A/b/../c/D/e.sot"
		       "/A/c/d.sot" "../c/d.sot"
                       "c/e/d.sot"))))

(deftest cl-0006-enough-namestring
    (labels ((test-path (path defaults)
	       (let* ((e-ns (enough-namestring path defaults))
		      (d1 (pathname-directory path))
		      (d2 (pathname-directory defaults))
		      (d3 (pathname-directory e-ns)))
		 (and (equalp (merge-pathnames e-ns defaults)
			      (merge-pathnames (parse-namestring path nil defaults)
					       defaults))
		      ;; If directories concide, the "enough-namestring"
		      ;; removes the directory. But only if the pathname is
		      ;; absolute.
		      (not (and (equal (first d1) ':absolute)
				(equalp d1 d2)
				d3)))))
	     (test-default+paths (default+paths)
	       (let ((defaults (first default+paths))
		     (paths (second default+paths)))
		 (every (lambda (path)
			 (handler-case (test-path path defaults)
			   (error (error) 'NIL)))
			paths))))
      (every #'test-default+paths *enough-namestring_tests*))
  t)

;;; Date: 10/08/2006 (Lars Brinkhoff)
;;; Fixed: 1/09/2006 (juanjo)
;;; Details:
;;;
;;;	ADJUST-ARRAY must signal a type error when the value of :FILL-POINTER is
;;;	not NIL and the adjustable array does not have a fill pointer
;;;

(deftest cl-0007-adjustable-array
    (loop for fp in '(nil t) collect
	  (loop for i in '(t nil 0 1 2 3) collect
		(and
		 (handler-case (adjust-array (make-array 3 :adjustable t :fill-pointer fp) 4
					     :fill-pointer i)
		   (type-error (c) nil)
		   (error (c) t))
		 t)))
  ((nil t nil nil nil nil) (t t t t t t)))

;;; Date: 09/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;	The namestring "." is improperly parsed, getting a file type of ""
;;;	Additionally we found it more convenient to have the _last_ dot mark
;;;	the file type, so that (pathname-type "foo.mpq.txt") => "txt"
;;;

(deftest cl-0008-parse-namestring
    (loop for (namestring name type) in
	  '(("." "." NIL) (".." "." "") (".foo" ".foo" NIL) (".foo.mpq.txt" ".foo.mpq" "txt")
	    ("foo.txt" "foo" "txt") ("foo.mpq.txt" "foo.mpq" "txt"))
	  unless (let ((x (parse-namestring namestring)))
		   (and (equal name (pathname-name x))
			(equal type (pathname-type x))
			(equal '() (pathname-directory x))))
	  collect namestring)
  ())

;;; Date: 28/09/2006
;;; Fixed: 10/10/2006
;;; Description:
;;;
;;;	Nested calls to queue_finalizer trashed the value of cl_core.to_be_finalized
;;;	The following code tests that at least three objects are finalized.
;;;
;;; Note: this test fails in multithreaded mode. GC takes too long!
#-ecl
(deftest cl-0009-finalization
    (let ((*all-tags* '()))
      (declare (special *all-tags*))
      (flet ((custom-finalizer (tag)
	       #'(lambda (o) (push tag *all-tags*))))
	(let ((a '()))
	  (dotimes (i 5)
	    (let ((x (cons i i)))
	      (si::set-finalizer x (custom-finalizer i))
	      (push x a))))
	(dotimes (j 100)
	  (dotimes (i 10000)
	    (cons 1.0 1.0))
	  (si::gc t)))
      (sort *all-tags* #'<))
  (0 1 2 3 4))


;;; Date: 8/10/2006 (Dustin Long)
;;; Fixed: 10/10/2006 (Dustin Long)
;;; Description:
;;;
;;;	Hash table iterators have to check that their argument is
;;;	really a hash table.
;;;

(deftest cl-0010-hash-iterator
    (loop for i in *mini-universe*
	  when (and (not (hash-table-p i))
		    (handler-case (progn (loop for k being the hash-keys of i) t)
		      (error (c) nil)))
	  collect (type-of i))
  nil)

;;; Date: 31/12/2006 (Richard M. Kreuter)
;;; Fixed: 5/1/2007 (Juanjo)
;;; Description:
;;;
;;;	The keyword :BACK does not work as expected when creating pathnames
;;;	and causes an error when at the beginning: (:RELATIVE :BACK)
;;;

(deftest cl-0011-make-pathname-with-back
    (loop for i from 0 to 200
       with l = (random 10)
       with x = (if (zerop l) 0 (random (1+ l)))
       with y = (if (= l x) 0 (random (- l x)))
       nconc (let* ((l (loop for i from 0 below l collect (princ-to-string i)))
		    (l2 (append (subseq l 0 y) '("break" :back) (subseq l y nil)))
		    (d1 (list* :absolute (subseq l2 0 x)))
		    (d2 (list* :relative (subseq l2 x nil)))
		    (d3 (list* :absolute l2))
		    (d4 (list* :relative l2))
		    (p1 (handler-case (make-pathname :directory d1)
			  (error (c) nil)))
		    (p2 (handler-case (make-pathname :directory d2)
			  (error (c) nil)))
		    (p3 (handler-case (make-pathname :directory d3)
			  (error (c) nil)))
		    (p4 (handler-case (make-pathname :directory d4)
			  (error (c) nil))))
	       (if (and p1 p2 p3 p4
                        ;; MERGE-PATHNAMES eliminates :BACK
			(equalp l (rest (pathname-directory (merge-pathnames p2 p1))))
                        ;; MAKE-PATHNAME does not eliminate :BACK
			(not (equalp l (rest (pathname-directory (make-pathname :directory d3)))))
			(not (equalp l (rest (pathname-directory (make-pathname :directory d4))))))
		   nil
		   (list (list l d1 d2 d3 d4 l2 x y)))))
  nil)

;;; Date: 11/03/2007 (Fare)
;;; Fixed: 23/03/2007 (Juanjo)
;;; Description:
;;;
;;;	COPY-READTABLE did not copy the entries of the "from" table
;;;	when a second argument, i.e. a "destination" table was supplied.
;;;

(deftest cl-0012-copy-readtable
    (let ((from-readtable (copy-readtable))
	  (to-readtable (copy-readtable))
	  (char-list '()))
      (dotimes (i 20)
	(let* ((code (+ 32 (random 70)))
	       (c (code-char code)))
	  (push c char-list)
	  (set-macro-character c
			       (eval `(lambda (str ch) ,code))
			       nil
			       from-readtable)))
      (copy-readtable from-readtable to-readtable)
      (loop for c in char-list
	    unless (and (eql (char-code c)
			     (let ((*readtable* from-readtable))
			       (read-from-string (string c))))
			(eq (get-macro-character c from-readtable)
			    (get-macro-character c to-readtable)))
	    collect c))
    nil)

;;; Date: 05/01/2008 (Anonymous, SF bug report)
;;; Fixed: 06/01/2008 (Juanjo)
;;; Description:
;;;
;;;	For a file linked as follows "ln -s //usr/ /tmp/foo", 
;;;	(truename #p"/tmp/foo") signals an error because //usr is
;;;	parsed as a hostname.
;;;

#-windows
(deftest cl-0013-truename
    (progn
      (si:system "rm -rf foo; ln -sf //usr/ foo")
      (prog1 (namestring (truename "./foo"))
        (si::system "rm foo")))
  "/usr/")

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 01/09/2008 (Juanjo)
;;; Description:
;;;
;;;	Inside the form read by #., recursive definitions a la #n=
;;;	and #n# were not properly expanded
;;;
(deftest cl-0014-sharp-dot
    (with-output-to-string (*standard-output*)
      (let ((*print-circle* t))
	(read-from-string "'#.(princ (list '#1=(1 2) '#1#))")))
  "(#1=(1 2) #1#)")

;;; Date: 30/08/2008 (Josh Elsasser)
;;; Fixed: 30/08/2008 (Josh Elsasser)
;;; Description:
;;;
;;;	A setf expansion that produces a form with a macro that also has
;;;	its own setf expansion does not giver rise to the right code.
;;;
(deftest cl-0015-setf-expander
    (progn
      (define-setf-expander triple (place &environment env)
	(multiple-value-bind (dummies vals newval setter getter)
	    (get-setf-expansion place env)
	  (let ((store (gensym)))
	    (values dummies
		    vals
		    `(,store)
		    `(let ((,(car newval) (/ ,store 3)))
		       (triple ,setter))
		    `(progn
		       (triple ,getter))))))
      (defmacro hidden (val)
	`(triple ,val))
      (defmacro triple (val)
	`(* 3 ,val))
      (prog1
	  (equalp (eval '(let ((foo 5))
			  (list foo (triple foo) (setf (triple foo) 6) foo (triple foo))))
		  (eval '(let ((foo 5))
			  (list foo (hidden foo) (setf (hidden foo) 6) foo (hidden foo)))))
	(fmakunbound 'hidden)
	(fmakunbound 'triple)))
  T)

;;; Date: 17/2/2009
;;; Fixed: 17/2/2009
;;; Description:
;;;
;;;	The defstruct form fails with an :include field that overwrites
;;;	a slot that is read only.
;;;
(deftest cl-0016-defstruct-include
    (progn
      (eval '(progn
              (defstruct cl-0016-a (a 1 :read-only t))
              (defstruct (cl-0016-b (:include cl-0016-a (a 2))))
              (defstruct (cl-0016-c (:include cl-0016-a (a 3 :read-only t))))))
      (values
       (handler-case (eval '(defstruct (cl-0016-d (:include cl-0016-a (a 2 :read-only nil)))))
         (error (c) t))
       (cl-0016-a-a (make-cl-0016-a))
       (cl-0016-b-a (make-cl-0016-b))
       (cl-0016-c-a (make-cl-0016-c))
       (handler-case (eval '(setf (cl-0016-c-a (make-cl-0016-c)) 3))
         (error (c) t))))
  t 1 2 3 t)

;;; Date: 9/11/2009
;;; Fixed: 9/11/2009
;;; Description:
;;;
;;;	LOAD does not work with special files (/dev/null)
;;;
(deftest cl-0017-load-special
    (handler-case (and (load #+(or windows mingw32) "NULL"
                             #-(or windows mingw32) "/dev/null")
                       t)
      (serious-condition (c) nil))
  t)

;;; Date: 16/11/2009 (Gabriel)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;	#= and ## reader macros do not work well with #.
;;;
(deftest cl-0018-sharp-eq
  (handler-case (values (read-from-string "(#1=(0 1 2) #.(length '#1#))"))
     (serious-condition (c) nil))
  ((0 1 2) 3))

;;; Date: 14/11/2009 (M. Mondor)
;;; Fixed: 20/11/2009 (Juanjo)
;;; Description:
;;;
;;;	FDEFINITION and SYMBOL-FUNCTION cause SIGSEGV when acting on NIL.
;;;
(deftest cl-0019-fdefinition
  (and (handler-case (fdefinition nil)
		     (undefined-function (c) t)
		     (serious-condition (c) nil))
       (handler-case (symbol-function nil)
		     (undefined-function (c) t)
		     (serious-condition (c) nil)))
  t)


;;; Date: 29/11/2009 (P. Costanza)
;;; Fixed: 29/11/2009 (Juanjo)
;;; Description:
;;;
;;;	Updating of instances is not triggered by MAKE-INSTANCES-OBSOLETE.
;;;
(deftest cl-0020-make-instances-obsolete
    (progn
      (defparameter *update-guard* nil)
      (defclass cl-0020-a () ((b :accessor cl-0020-a-b :initarg :b)))
      (let ((*a* (make-instance 'cl-0020-a :b 2)))
        (defmethod update-instance-for-redefined-class :before
            ((instance standard-object) added-slots discarded-slots property-list
             &rest initargs)
          (setf *update-guard* t))
        (and (null *update-guard*)
             (progn (cl-0020-a-b *a*) (null *update-guard*))
             (progn (make-instances-obsolete (find-class 'cl-0020-a))
                    (null *update-guard*))
             (progn (cl-0020-a-b *a*) *update-guard*)
             (progn (setf *update-guard* nil)
                    (defclass cl-0020-a () ((b :accessor cl-0020-a-b :initarg :b)))
                    (cl-0020-a-b *a*)
                    *update-guard*)
             t)))
  t)

;;; Date: 25/03/2009 (R. Toy)
;;; Fixed: 4/12/2009 (Juanjo)
;;; Description:
;;;
;;;	Conversion of rationals into floats is done by truncating, not by
;;;	rounding, what implies a loss of accuracy.
;;;
(deftest cl-0021-ratio-to-float
    ;; The test builds a ratio which is very close to 1 but which is below it
    ;; If we truncate instead of rounding the output will not be 1 coerced
    ;; to that floating point type.
    (loop for type in '(short-float single-float double-float long-float)
       for bits = (float-precision (coerce 1 type))
       do (loop for i from (+ bits 7) to (+ bits 13)
             nconc (loop with value = (ash 1 i)
                      with expected = (coerce 1 type)
                      for j from 0 to 10
                      for x = (- value j)
                      for r = (/ (1- x) x)
                      for f1 = (coerce r type)
                      for f2 = (- (coerce (- r) type))
                      unless (and (= f1 expected) (= f2 expected))
                      collect (list type r))))
  nil)

;;; Date: 06/04/2010 (M. Kocic)
;;; Fixed: 4/12/2009
;;; Description:
;;;
;;;	Inspection of structs is broken due to undefined inspect-indent
;;;
(deftest cl-0022-inspect-struct
    (let ((*query-io* (make-string-input-stream "q
")))
      (defstruct st1 p1)
      (let ((v1 (make-st1 :p1 "tttt")))
        (handler-case (progn (inspect v1) t)
          (error (c) nil))))
  t)
