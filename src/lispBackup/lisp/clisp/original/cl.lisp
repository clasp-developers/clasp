#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")

(defmacro ENGLISH (x) x)

;;; Iterate  --  Public
;;;
;;;    The ultimate iteration macro...
;;;
(defmacro iterate (name binds &body body)
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
  (dolist (x binds)
    (unless (and (listp x)
		 (= (length x) 2))
      (error "Malformed iterate variable spec: ~S." x)))
  
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))


;;; Once-Only  --  Interface
;;;
;;;    Once-Only is a utility useful in writing source transforms and macros.
;;; It provides an easy way to wrap a let around some code to ensure that some
;;; forms are only evaluated once.
;;;
(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
	   ((specs specs)
	    (body body))
    (if (null specs)
	`(progn ,@body)
	(let ((spec (first specs)))
	  (when (/= (length spec) 2)
	    (error "Malformed Once-Only binding spec: ~S." spec))
	  (let ((name (first spec))
		(exp-temp (gensym)))
	    `(let ((,exp-temp ,(second spec))
		   (,name (gensym "OO-")))
	       `(let ((,,name ,,exp-temp))
		  ,,(frob (rest specs) body))))))))


(defun whitespacep (char &optional (rt *readtable*))
  (eq (_char-type char rt) :whitespace))
;!!!  (test-attribute char whitespace rt))



;;; WITH-ARRAY-DATA  --  Interface
;;;
;;;    Checks to see if the array is simple and the start and end are in
;;; bounds.  If so, it proceeds with those values.  Otherwise, it calls
;;; %WITH-ARRAY-DATA.  Note that there is a derive-type method for
;;; %WITH-ARRAY-DATA.
;;;
(defmacro with-array-data (((data-var array &key (offset-var (gensym)))
			    (start-var &optional (svalue 0))
			    (end-var &optional (evalue nil)))
			   &rest forms)
  "Given any Array, binds Data-Var to the array's data vector and Start-Var and
  End-Var to the start and end of the designated portion of the data vector.
  Svalue and Evalue are any start and end specified to the original operation,
  and are factored into the bindings of Start-Var and End-Var.  Offset-Var is
  the cumulative offset of all displacements encountered, and does not
  include Svalue."
  (once-only ((n-array array)
	      (n-svalue svalue)
	      (n-evalue evalue))
    `(multiple-value-bind
	 (,data-var ,start-var ,end-var ,offset-var)
           (let ((,n-array ,n-array))
             (declare (type (simple-array * (*)) ,n-array))
             ,(once-only ((n-len `(length ,n-array))
      		    (n-end `(or ,n-evalue ,n-len)))
      	  `(if (<= ,n-svalue ,n-end ,n-len)
      	       (values ,n-array ,n-svalue ,n-end 0)
      	       (%with-array-data ,n-array ,n-svalue ,n-evalue))))
       (declare (ignorable ,offset-var))
       ,@forms)))

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  "Examine the substring of string delimited by start and end
  (default to the beginning and end of the string)  It skips over
  whitespace characters and then tries to parse an integer.  The
  radix parameter must be between 2 and 36."
  (with-array-data ((string string)
		    (start start)
		    (end (or end (length string))))
    (let ((index (do ((i start (1+ i)))
		     ((= i end)
		      (if junk-allowed
			  (return-from parse-integer (values nil end))
			  (error "No non-whitespace characters in number.")))
		   (declare (fixnum i))
		   (unless (whitespacep (char string i)) (return i))))
	  (minusp nil)
	  (found-digit nil)
	  (result 0))
      (declare (fixnum index))
      (let ((char (char string index)))
	(cond ((char= char #\-)
	       (setq minusp t)
	       (incf index))
	      ((char= char #\+)
	       (incf index))))
      (loop
	(when (= index end) (return nil))
	(let* ((char (char string index))
	       (weight (digit-char-p char radix)))
	  (cond (weight
		 (setq result (+ weight (* result radix))
		       found-digit t))
		(junk-allowed (return nil))
		((whitespacep char)
		 (do ((jndex (1+ index) (1+ jndex)))
		     ((= jndex end))
		   (declare (fixnum jndex))
		   (unless (whitespacep (char string jndex))
		     (error "There's junk in this string: ~S." string)))
		 (return nil))
		(t
		 (error "There's junk in this string: ~S." string))))
	(incf index))
      (values
       (if found-digit
	   (if minusp (- result) result)
	   (if junk-allowed
	       nil
	       (error "There's no digits in this string: ~S" string)))
       index))))

#|!!!R
(defun present (s p)
	(multiple-value-bind (sym st) (find-symbol (symbol-name s) p)
	  (and (eq s sym) (memq st '(:internal :external)))))
|#
      

(import
  '(_get-package memq listify _seq-iterate _concs ext:string-concat
    _pr)
  (find-package "CL"))
              
  
;!!!(setq *package* (find-package "CL"))

(setq *_trace_* t)

(defvar _*gentemp-counter* 0)

(defun gentemp (&optional (x "T") (pack *package*))
  (do ()
      (nil)
    (multiple-value-bind (sym st) (intern (ext:string-concat x (write-to-string (incf _*gentemp-counter*))) pack)
      (if (null st) (return sym)))))

(defun _elts (seq i)
  (if seq (cons (elt (car seq) i) (_elts (cdr seq) i))))

(defun _every (p seq i n)
  (if (< i n) (if (apply p (_elts seq i)) (_every p seq (1+ i) n))
              t))

(defun _some (p seq i n)
  (if (< i n) (cond ((apply p (_elts seq i)))
                    ((_some p seq (1+ i) n)))))

(defun every (p &rest seq)
  (_every p seq 0 (apply #'min (mapcar #'length seq))))

(defun some (p &rest seq)
  (_some p seq 0 (apply #'min (mapcar #'length seq))))

(defun notany (p &rest seq)
  (not (apply #'some p seq)))

(defun notevery (p &rest seq)
  (not (apply #'every p seq)))

#|!!! buggy    
(defun _rotatef-2 (r n forms)
  (if r (let ((x (car r)))
          `(let ((,n ,(fifth x)))
             ,(_rotatef-2 (cdr r) (car (third x)) (cons (fourth x) forms))))
        `(progn ,@(nreverse forms)
                nil)))
  
(defmacro rotatef (&body p)
   (if p (let* ((r (_setf-expansions p))
                (z (car (last r))))
           (_rotatef-2 r (car (third z)) nil))))
|#           

(defun devalue-form (form)
  (if (eq (car form) 'VALUES) (cdr form) (list form))
)
           
(defmacro rotatef (&rest args &environment env)
  (when (null args) (return-from rotatef NIL))
  (when (null (cdr args)) (return-from rotatef `(PROGN ,(car args) NIL)))
  (do* ((arglist args (cdr arglist))
        (res (list 'let* nil nil)) lf
        (tail (cdr res)) bindlist stores lv fv)
       ((null arglist)
        (setf (second res) (nreverse bindlist)
              (second (third res)) lv
              (cdr tail) (nconc (nreverse stores) (devalue-form lf))
              (cdr (last res)) (list nil))
        res)
    (multiple-value-bind (vr vl sv se ge)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nreconc (mapcar #'list vr vl) bindlist))
      (setf (cadr tail) (list 'MULTIPLE-VALUE-BIND lv ge nil))
      (setq tail (cddadr tail))
      (if (null fv)
        (setq fv sv)
        (setq stores (revappend (devalue-form lf) stores))
      )
      (setq lv sv lf se))))
           

(defun _shiftf (p n r store-forms z)
  (if p (multiple-value-bind (vars vals stores store-form access-form) (get-setf-expansion (car p))
				(declare (ignore vars vals))
          `(let ((,r ,access-form))
             ,(_shiftf (cdr p) n (car stores) (append store-forms (list store-form)) z)))
        `(let ((,r ,n))
           ,@store-forms
           ,z)))

(defmacro shiftf (&body p)
  (let ((r (gensym)))
    (_shiftf (butlast p) (car (last p)) r nil r)))


;;; mapappend is like mapcar except that the results are appended together:
(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))
              
(defmacro type-specifier-atom (type)
  (if (atom type) type (car type)))              
  
(defmacro elt-slice (sequences n)
	`(mapcar #'(lambda (seq) (elt seq ,n)) ,sequences))
	
(defmacro map-for-effect (function sequences)
  `(do ((seqs more-sequences (cdr seqs))
	(min-length (length first-sequence)))
       ((null seqs)
	(do ((index 0 (1+ index)))
	    ((= index min-length) nil)
	  (apply ,function (elt-slice ,sequences index))))
     (declare (fixnum min-length))
     (let ((length (length (car seqs))))
       (declare (fixnum length))
       (if (< length min-length)
	   (setq min-length length)))))  

	   #|	   !!!!
(defmacro map-to-list (function sequences)
  `(do ((seqs more-sequences (cdr seqs))
	(min-length (length first-sequence)))
       ((null seqs)
	(let ((result (list nil)))
	  (do ((index 0 (1+ index))
	       (splice result))
	      ((= index min-length) (cdr result))
	    (declare (fixnum index))
	    (setq splice
		  (cdr (rplacd splice
			       (list (apply ,function (elt-slice ,sequences
								 index)))))))))
     (declare (fixnum min-length))
     (let ((length (length (car seqs))))
       (declare (fixnum length))
       (if (< length min-length)
	   (setq min-length length)))))	   
	   |#
	   
	   
              


;;; push-on-end is like push except it uses the other end:

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
      ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

(defun get-properties (p i)
  (if p (let ((a (car p))
              (r (cdr p)))
          (if (memq a i) (values a (car r) p)
                          (get-properties (cdr r) i)))
        (values nil nil nil)))

;;; mapplist is mapcar for property lists:

(defun mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x) (cadr x))
            (mapplist fun (cddr x)))))

(defconstant char-code-limit 65536)
(defconstant base-char-code-limit char-code-limit)

(defun character (x)
  (if (characterp x) x
      (if (and (or (stringp x) (symbolp x))
               (= (length (string x)) 1))
        (char (string x) 0)
        (error 'type-error :datum x))))

(defun graphic-char-p (ch)
  (>= (char-code ch) (char-code #\Space)))

(defun prin1 (x &optional stm)
  (write x :stream stm :escape t))

(defun print (x &optional stm)
  (prog2 (terpri stm)
         (prin1 x stm)
         (write-char #\Space stm)))

(defun write-line (string &optional s &key (start 0) end)
  (prog1 (write-string string s :start start :end end)
         (terpri s)))

(defun keywordp (x)
  (eq (type-of x) 'keyword))
  
(defmacro with-standard-io-syntax (&rest forms)
  `(let ((*package* (find-package 'cl-user))
         (*print-array* t)
         (*print-base 10)
         (*print-case :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pprint-dispatch* nil) ;!!!
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-rigth-margin* nil)
         (*read-base* 10)
         (*read-default-float-format* 'single-float)
         (*read-eval* t)
         (*read-suppress* nil)
         (*readtable* (copy-readtable nil))) ;!!!
     ,@forms))

(defun find-all-symbols (s &aux (str (string s)) r)
  (dolist (p (list-all-packages) r)
    (let ((sym (find-symbol str p)))
      (if sym (pushnew sym r)))))

(defun %in-package (name &rest rest &key (nicknames nil n-p) (use nil u-p) (case-sensitive nil c-p) case-inverted)
		(declare (ignore case-sensitive case-inverted c-p))
  (let ((p (or (find-package (string name))
               (apply #'make-package name :allow-other-keys t rest))))
    (if n-p (rename-package p (package-name p) nicknames))
    (when u-p
      (use-package use p)
      (dolist (u (package-use-list p))
        (unless (memq u use)
          (unuse-package u p))))
    p))      

(defmacro defpackage (name &rest options)
  (let (size
        doc
        nicks
        shadow-list
        shadowing-list
        (use-list '(cl))
        imports
        interns)
			(declare (ignore size doc shadow-list shadowing-list imports interns))
    (dolist (opt options)
      (case (car opt)
        (:size)  ;!!! ignore
        (:documentation)   ;!!! ignore
        (:nicknames (setq nicks (append nicks (cdr opt))))
        (:shadow)
        (:use (setq use-list (cdr opt)))
        (:import-from)
        (:intern)
        (:export)
        (t (error t "Invalid defpackage option"))))
    `(EVAL-WHEN (LOAD COMPILE EVAL)
			 (prog1 (cond ((find-package ',name))
										((make-package ',name :nicknames ',nicks :use ',use-list)))))))

(defpackage "COMMON-LISP-USER"
  (:nicknames "CL-USER" "USER")
  (:use "CL" "EXT"))

(defpackage "I18N"
  (:use "CL" "EXT"))

(defpackage "GRAY"
  (:use "CL" "EXT"))

#|
(defun _substring (x s)
  (let ((n (length x)))
    (if (<= n (length s))
      (or (string= x (subseq s 0 (length x))) (_substring x (subseq s 1))))))


(defun apropos (s &optional pack)
  (let ((name (string s)))
    (if pack (do-symbols (sym pack)
               (if (_substring name (symbol-name sym)) (print sym)))
             (do-all-symbols (sym pack)
               (if (_substring name (symbol-name sym)) (print sym)))))
  (values))

(defun apropos-list (s &optional pack &aux p)
  (let ((name (string s)))
    (if pack (do-symbols (sym pack)
               (if (_substring name (symbol-name sym)) (push sym p)))
             (do-all-symbols (sym pack)
               (if (_substring name (symbol-name sym)) (push sym p)))))
  p)
|#

(defun conjugate (z)
  (complex (realpart z) (- (imagpart z))))

(defun schar (v i)
  (if (simple-string-p v)
    (aref v i)
    (error 'type-error :datum v :expected-type 'simple-string)))

(defun store-schar (s i n)
  (if (simple-string-p s)
    (setf (aref s i) n)
    (error 'type-error :datum s :expected-type 'simple-string)))

(defmacro while (c &body body)
  `(do ()
       ((not ,c))
     ,@body))

(defmacro forever (&body body)
  `(while t ,@body))

#|!!!R	
(defun make-hash-table (&key (test 'eql) size (rehash-size 1) (rehash-threshold 1) initial-contents
                             key-type value-type warn-if-needs-rehash-after-gc weak)
		(declare (ignore size key-type value-type warn-if-needs-rehash-after-gc weak))
	(let ((ht (_make-hash-table (labels ((_tests (x)
                               (if x (let ((a (car x)))
                                       (list* (cons a a)
                                              (cons (symbol-function a) a)
                                              (_tests (cdr x)))))))
                      (let ((r (assoc test (_tests '(eq eql equal equalp fasthash-eq stablehash-eq fasthash-eql stablehash-eql fasthash-equal stablehash-equal)))))
                        (if r (cdr r)
                              (err))))
                    rehash-size rehash-threshold)))
		(dolist (e initial-contents ht)
			(setf (gethash (car e) ht) (cdr e)))))
|#

(defun (setf gethash) (v key ht &optional def)
		(declare (ignore def))
  (puthash key ht v))

(defmacro with-hash-table-iterator ((name ht) &body body)
  (let ((var (gensym)))
    `(let ((,var (hash-table-iterator ,ht)))
       (macrolet ((,name () '(hash-table-iterate ,var)))
         ,@body))))

(defun maphash (f ht)
  (with-hash-table-iterator (next ht)
    (forever (multiple-value-bind (more key value) (next)
               (if more (funcall f key value)
                        (return))))))



(defun union (p q &rest rest &key key (test #'eql) test-not &aux r)
		(declare (ignore test test-not))
  (setq key (_key key))
  (dolist (x p (nconc r q))
    (unless (apply #'member (funcall key x) q rest)
      (push x r))))

(defun nunion (p q &rest rest &key key (test #'eql) test-not)
		(declare (ignore key test test-not))
  (apply #'union p q rest)) ;!!! make more effective

(defun subsetp (p q &rest rest &key key (test #'eql) test-not)
		(declare (ignore test test-not))
  (setq key (_key key))
  (dolist (x p t)
    (if (not (apply #'member (funcall key x) q rest)) (return nil))))

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
		(declare (ignore key test test-not))
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
		(declare (ignore key test test-not))
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'nset-difference list2 list1 rest)))
         
(defun make-string-output-stream (&key (element-type 'character) (line-position 0))
  (if (null element-type) (setq element-type 'character)) ;!!! CLISP's incorrect arg
	(make-string-push-stream (make-string 0 :element-type element-type) line-position))
	

(defun lisp-implementation-type ()
  "Ufasoft LISP")

(defun short-site-name ()
  (machine-instance))

(defun long-site-name ()
  (short-site-name))

(defvar *compile-print* nil)
(defvar *compile-verbose* nil)

(defun package-iterator-function (pack-list symbol-types)
  (let ((iterstates
          (mapcar #'(lambda (pack) (package-iterator pack symbol-types))
                  (if (listp pack-list) pack-list (list pack-list)))))
    ; The iterstates list is cdr'ed down during the iteration.
    #'(lambda ()
        (loop
          (if iterstates
            (multiple-value-bind (more symb acc)
                (package-iterate (car iterstates))
              (if more
                (return (values more symb acc (svref (car iterstates) 4)))
                (pop iterstates)
            ) )
            (return nil))))))

#|!!!
(defmacro with-package-iterator ((name pack-list &rest types) &body body)
  (unless types
    (error-of-type 'source-program-error
      (ENGLISH "missing symbol types (~S/~S/~S) in ~S")
      ':internal ':external ':inherited 'with-package-iterator))
  (dolist (symboltype types)
    (case symboltype
      ((:INTERNAL :EXTERNAL :INHERITED))
      (t (error-of-type 'source-program-error
           (ENGLISH "~S: flag must be one of the symbols ~S, ~S, ~S, not ~S")
           'with-package-iterator ':internal ':external ':inherited symboltype))))
  (let ((iterfun (gensym "WPI")))
    `(let ((,iterfun (package-iterator-function ,pack-list ',(remove-duplicates types))))
       (macrolet ((,name () '(funcall ,iterfun)))
         ,@body))))
|#

(defconstant y-or-n '((#\N) . (#\Y)))
(defconstant yes-or-no '(("no") . ("yes")))

(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string " (y/n) " *query-io*)
  )
  (let ((localinfo y-or-n))
    (loop
      (let ((line (string-left-trim " " (read-line *query-io*))))
        (when (plusp (length line))
          (let ((first-char (char-upcase (char line 0))))
            (when (member first-char (car localinfo)) (return nil))
            (when (member first-char (cdr localinfo)) (return t)))))
      (terpri *query-io*)
      (write-string "Please answer with y or n : " *query-io*))))

(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string " (yes/no) " *query-io*)
  )
  (let ((localinfo yes-or-no))
    (loop
      (clear-input *query-io*)
      (let ((line (string-trim " " (read-line *query-io*))))
        (when (member line (car localinfo) :test #'string-equal) (return nil))
        (when (member line (cdr localinfo) :test #'string-equal) (return t)))
      (terpri *query-io*)
      (write-string "Please answer with yes or no : " *query-io*))))

(defun file-string-length (stm obj)
		(declare (ignore stm obj))
  nil)

(defmacro define-compiler-macro (&whole form name args &body body)
  (declare (ignore name args body))
  (multiple-value-bind (expansion name lambdalist docstring)
      (make-macro-expansion (cdr form) 'strip-funcall-form)
    (declare (ignore lambdalist))
    `(EVAL-WHEN (COMPILE LOAD EVAL)
      ,@(when docstring
         `((SYSTEM::%SET-DOCUMENTATION ',name 'COMPILER-MACRO ,docstring)))
      (setf (compiler-macro-function ',name) ,expansion)
      ',name)))

#|
(defmacro define-symbol-macro (symbol expansion)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      (ENGLISH "~S: the name of a symbol macro must be a symbol, not ~S")
      'define-symbol-macro symbol
  ) )
  `(LET ()
     (EVAL-WHEN (COMPILE LOAD EVAL)
       (CHECK-NOT-SPECIAL-VARIABLE-P ',symbol)
       (MAKUNBOUND ',symbol)
       (SYSTEM::SET-SYMBOL-VALUE ',symbol (SYSTEM::MAKE-SYMBOL-MACRO ',expansion))
     )
     ',symbol
   )
)
|#


  
(macrolet ((frob (name result access src-type &optional typep)
	`(DEFUN  ,name (object ,@(if typep '(type) ()))
    (DO* ((index 0 (1+ index))
				  (length (length (the ,(case src-type
																	(:list 'list)
																	(:vector 'vector))
										 object)))
					(result ,result))
				 ((= index length) result)
			(declare (fixnum length index))
			(SETF (,access result index) ,(case src-type
																		 (:list '(pop object))
																		 (:vector '(aref object index))))))))

  (frob list-to-string* (make-string length) schar :list)

  (frob list-to-bit-vector* (make-array length :element-type '(mod 2)) sbit :list)

  (frob list-to-vector* (make-sequence-of-type type length) aref :list t)

  (frob vector-to-vector* (make-sequence-of-type type length) aref :vector t)

  (frob vector-to-string* (make-string length) schar :vector) (frob vector-to-bit-vector* (make-array length :element-type '(mod 2))
	sbit :vector)
)

#|
(defun vector-to-list* (object)
  (let ((result (list nil))
		(length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
				 (splice result (cdr splice)))
				((= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))
      
      |#
  
(defun specifier-type (spec)
	spec)

(defun csubtypep (spec1 spec2)
	(subtypep spec1 spec2))
	
(defun string-to-simple-string*	(seq)
	(copy-seq seq))
	


(defun file-stream-p (x)
	(eq (type-of x) 'file-stream))

(defun synonym-stream-p (x)
	(eq (type-of x) 'synonym-stream))

(defun broadcast-stream-p (x)
	(eq (type-of x) 'broadcast-stream))

(defun concatenated-stream-p (x)
	(eq (type-of x) 'concatenated-stream))

(defun two-way-stream-p (x)
	(eq (type-of x) 'two-way-stream))

(defun echo-stream-p (x)
	(eq (type-of x) 'echo-stream))

(defun string-stream-p (x)
	(eq (type-of x) 'string-stream))

(defun _as-string (x)
  (with-output-to-string (stm)
    (prin1 x stm)))

;!!!  (if (= (length args) 1) (_err1 cnd code (_as-string (car args)))
;                          (error t "Invalid _ERR")))

#|!!!R

(defun invalid-method-error (meth formcont &rest args)
		(declare (ignore meth formcont args))
  (err))

(defun method-combination-error (formcont &rest args)
		(declare (ignore formcont args))
  (err))
|#



;;; NORMALIZE-TYPE normalizes the type using the DEFTYPE definitions.
;;; The result is always a list.
(defun normalize-type (type &aux tp i )
  ;; Loops until the car of type has no DEFTYPE definition.
  (loop
    (if (atom type)
        (setq tp type i nil)
        (setq tp (car type) i (cdr type)))
    (if (get tp 'deftype-expander)
        (setq type (apply (get tp 'deftype-expander) i))
        (return-from normalize-type (if (atom type) (list type) type)))))
        

;(defconstant ELTYPE_T 0)
;(defconstant ELTYPE_BIT 1)
;(defconstant ELTYPE_CHARACTER 2)
        
(defun _eltype-code (x)
  (case x
  	(bit 'ELTYPE_T)
  	(character 'ELTYPE_CHARACTER)
  	((t) 'ELTYPE_T)
  	(nil nil)
  	(t (multiple-value-bind (low high)
  	   	 	 (subtype-integer x)
  	   	 (if (and (integerp low) (> low 0))
  	   	 	 	(let ((len (integer-length high)))
  	   	 	 		(cond ((<= len 1) 'ELTYPE_BIT)
  	   	 	 		      (t 'ELTYPE_T)))
  	   	 	 	(if (subtypep x 'character) 'ELTYPE_CHARACTER 'ELTYPE_T))))))	   	 	 		
  	      
(defun coerce (x type)
  (if (typep x type)
     x
     (progn (setq type (expand-deftype type))
						(if (clos::potential-class-p type) (setq type (class-name type)))
						(if (atom type) (setq type (list type)))
						(case (car type)
							((t) x)
							(and (if (null (cdr type))
											x
											(let ((n (coerce x (cadr type))))
												(if (typep n type) n
																					 (err)))))    
							((character string-char) (character x))
							((float short-float single-float double-float loang-float) (float x))
							(complex (complex x))
							(function (let ((f (fboundp x)))
							            (if (function-macro-p f) (setq f (function-macro-function f))))) ;!!! need lamda
					;    (array simple-array vector simple-vector string simple-string base-string simple-base-string bit-vector simple-bit-vector)
							(t (_coerce-seq x (car type) t))))))

     

(defvar *random-state* (make-random-state t))

