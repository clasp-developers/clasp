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

(setq *print-pretty-fill* nil)

(defvar *internal-compiled-file-type* "fas")

(defvar custom:*compiled-file-types* (list *internal-compiled-file-type*))

(defvar *endless-loop-code* #12Y(00 00 00 00 00 00 00 00 11 16 1B 7E))
(defvar *constant-initfunction-code* #13Y(00 00 00 00 00 00 00 00 00 01 C5 19 01))

(defun make-constant-initfunction (x)
	(%make-closure 'constant-initfunction *constant-initfunction-code* (list x) nil nil nil))

(defun constant-initfunction-p (x)
  (and (closurep x)
			 (compiled-function-p x)
			 (eq (closure-name x) 'constant-initfunction)
			 (eq (closure-codevec x) *constant-initfunction-code*)))

(defun %compiled-function-p (x)
  (or (compiled-function-p x) (clos::funcallable-instance-p x)))

(defun make-closure (&key name code constants seclass lambda-list documentation jitc-p)
		(declare (ignore jitc-p))
  (sys::%make-closure name (sys::make-code-vector code) constants seclass
                      lambda-list documentation))

(%putd 'quit #'exit)

(defun current-language ()
  'ENGLISH)
  
(defun elastic-newline (&optional stm)
  (terpri stm)) ;!!!TODO

(defun function-block-name (funname)
  (if (atom funname) funname (second funname)))
  
(defun package-case-inverted-p (p)
		(declare (ignore p))
  nil)
  
(defun package-case-sensitive-p (p)
		(declare (ignore p))
	nil)
  
(defun lib-directory ()
  (pathname ".\\"))
  
(defun write-spaces (n &optional stm)
  (dotimes (i n nil)
    (princ #\Space stm)))
  
(defun symbol-stream (sym &optional dir)
		(declare (ignore dir))
  (symbol-value sym))
  
(defun function-lambda-expression (obj)
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (values nil nil (sys::%record-ref obj 0)))
        ((sys::subr-info obj)
         (values nil nil (sys::subr-info obj)))
        ((compiled-function-p obj) ; compiled closure?
         (let* ((name (sys::%record-ref obj 0))
                (def (get name 'sys::definition)))
           (values (when def (cons 'LAMBDA (cddar def))) t name)))
        ((sys::closurep obj) ; interpreted closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; lambda-expression without docstring
                 (vector ; environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8)); denv
                 (sys::%record-ref obj 0))))) ; name
  
(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil))
  
(defvar *prin-indentation*)
(defvar *prin-linelength* 79)


(defun format-tab (stream colon-modifier atsign-modifier &optional colnum colinc)
  (setq colnum (or colnum 1))
  (setq colinc (or colinc 1))
  (let* ((new-colnum (+ (max colnum 0)
                        (if (and colon-modifier (boundp '*prin-indentation*))
                            *prin-indentation* 0)))
         (new-colinc (max colinc 1)) ; >0
         (pos (sys::line-position stream))) ; actual position, fixnum>=0 or NIL
    (if atsign-modifier
      (+ new-colnum
         (if pos (mod (- (+ pos new-colnum)) new-colinc)
                 0))
      (if pos
        (if (< pos new-colnum)
          (- new-colnum pos)
          (+ colinc (mod (- new-colnum pos) (- new-colinc))))
        2))))
  
(defun string-both-trim (character-bag-left character-bag-right string invertp)
		(declare (ignore invertp))
 (let ((l (length string)))
   (do ((i 0 (1+ i)))
       (nil)
     (when (or (= i l)
               (not (find (char string i) character-bag-left)))
       (do ((j l (1- j)))
           (nil)
         (when (or (= i j)
                   (not (find (char string (1- j)) character-bag-right)))
           (return-from string-both-trim (_rev-list-to-string
                                           (let ((r))
                                             (dotimes (k (- j i) r)
                                               (push (char string (+ i k)) r)))))))))))

(defvar *current-source-file* nil)

(defvar *toplevel-denv* nil)

(defun function-side-effect (fun)
		(declare (ignore fun))
  '(T T T)) ;;;!!! '(T . T))
  
(defun symbol-value-lock (s)
		(declare (ignore s))
	nil)
	
(defun string-width (s)
	(length s))	
  
(defun maplap (fun &rest rest)
	(apply #'append (apply #'maplist fun rest)))
	
(defun mapcap (fun &rest lists)
  (apply #'append (apply #'mapcar fun lists)))	
	
(defun concat-pnames (obj1 obj2)
  (let ((str (ext:string-concat (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
      (intern (subseq str 1) *keyword-package*)
      (intern str))))
	

(defun check-function-name (funname caller)
  (do () ((function-name-p funname) funname)
    (setq funname (check-value nil (coerce-to-condition
                                    (TEXT "~s: ~s is not a function name")
                                    (list caller funname)
                                    'check-function-name
                                    'simple-source-program-error)))))

                                    

(defun %defclcs (conds)
 	(setq *_conds* conds))
 	
(defun convert-simple-condition (c)
	(let ((e (find c *_conds* :test #'eq :key #'car)))
 	  (if e (cdr e) c)))
 	
(defun %defgray (v)
  (setq <fundamental-stream> (svref v 0)
				<fundamental-input-stream> (svref v 1)
				<fundamental-output-stream> (svref v 2)))
 	
 	
(defvar *recursive-error-count* 0) 	
                                        
(defun NOTE-NEW-STRUCTURE-CLASS ()
	)
	
(defun NOTE-NEW-STANDARD-CLASS ()
	)
	
(defun %copy-simple-vector (v)
	(copy-seq v))
	
(defun random-posfixnum ()
  (random (1+ most-positive-fixnum)))

(defvar *documentation* (make-hash-table :test 'eq :size 1000))

(defun argv ()
  (concatenate 'vector *args*))	

(defun encodingp (i)
		(declare (ignore i))
  nil)

(defun base-char-p (ch)
  (and (characterp ch) (< (char-code ch) 256)))
  
(defun sys::double-float-p (x)
	(floatp x))

(defun sys::long-float-p (x)
	(floatp x))

(defun sys::short-float-p (x)
	(floatp x))

(defun sys::single-float-p (x)
	(floatp x))
	
(defun stream-element-type (s)
	 (built-in-stream-element-type s))

(defun sequencep (x)
	(or (listp x) (vectorp x)))

#|!!!R
(defmacro do-external-symbols ((var &optional (pack '*package*) res) &body body) ; redefined in defs.lisp
  `(dolist (,var (_get-external-symbols (_get-package ,pack)) ,res)
     ,@body))
|#

(defmacro do-external-symbols ((var &optional (pack '*package*) res) &body body) ; redefined in defs.lisp
  `(progn
     (map-external-symbols (lambda (,var)
                           ,@body)
                         ,pack)
     ,res))


(defun ext:re-export (from to)
	(do-external-symbols (sym from)
		(import sym to)
		(export sym to)))
		
(defun package-shortest-name (p &aux (r (package-name p)))
  (dolist (n (package-nicknames p) r)
		(when (< (length n) (length r))
  		(setq r n))))  	
		
(defmacro destructuring-bind (&whole whole-form lambdalist form &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (if declarations (setq declarations `((DECLARE ,@declarations))))
	(let ((%whole-form whole-form) (%proper-list-p nil)
    	(%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form nil))
      (analyze1 lambdalist '<DESTRUCTURING-FORM> 'destructuring-bind '<DESTRUCTURING-FORM>)
      (let ((lengthtest (make-length-test '<DESTRUCTURING-FORM> 0))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (DESTRUCTURING-ERROR <DESTRUCTURING-FORM>
                                    '(,%min-args . ,(if %restp nil %arg-count))
               )
               ,mainform
        ) )  )
        `(LET ((<DESTRUCTURING-FORM> ,form)) ,mainform)
) ) ) )

#|       
(defmacro destructuring-bind (lam exp &rest forms)
  `(let* ((_args ,exp) ,@(_macro-lam-list lam '_args))
     ,@forms))
     |#

		

(defvar custom:*suppress-check-redefinition*		t)


(defun sys::preliminary-p (name)
	(and (consp name) (eq (car name) 'ext:preliminary)))

(defun sys::fbound-string (sym)
    (cond ((special-operator-p sym) (TEXT "special operator"))
          ((macro-function sym)
           (if (not (and (sys::closurep (macro-function sym))
                         (sys::preliminary-p (sys::closure-name
                                              (macro-function sym)))))
             (TEXT "macro")
             nil))
          ((fboundp sym)
           (if (not (and (sys::closurep (symbol-function sym))
                         (sys::preliminary-p (sys::closure-name
                                              (symbol-function sym)))))
             (TEXT "function")
             nil))))

		
(defun sys::check-redefinition (symbol caller what)
    (let ((cur-file *current-source-file*)
          ;; distinguish between undefined and defined at top-level
          (old-file (getf (gethash symbol *documentation*) 'sys::file)))
      (unless (or custom:*suppress-check-redefinition*
                  (equalp old-file cur-file)
                  (and (pathnamep old-file) (pathnamep cur-file)
                       (equal (pathname-name old-file)
                              (pathname-name cur-file))))
        (sys::check-package-lock
         caller
         (cond ((atom symbol) (symbol-package symbol))
               ((function-name-p symbol) (symbol-package (second symbol)))
               ((mapcar #'(lambda (obj) ; handle (setf NAME) and (eql NAME)
                            (let ((oo (if (atom obj) obj (second obj))))
                              (when (symbolp oo)
                                (symbol-package oo))))
                        symbol)))
         symbol)
        (when what ; when not yet defined, `what' is NIL
          (warn (TEXT "~a: redefining ~a ~s in ~a, was defined in ~a")
                caller what symbol (or cur-file "top-level")
                (or old-file "top-level")))
        (system::%set-documentation symbol 'sys::file cur-file))))
		

(import 'mapcap 'ext)
(export 'mapcap 'ext)


(defun allow-read-eval (stm x)
		(declare (ignore stm x))
  t)
  
(defun report-one-new-value-string ()
  (TEXT "You may input a new value for ~S."))
  
(defun report-no-new-value-string ()
  (TEXT "Retry"))

(defun report-new-values-string ()
  (TEXT "You may input new values for ~S."))

  
       
(defun CLOSURE-SET-SECLASS (clos seclass)
		(declare (ignore clos))
	seclass)
	
(defun %find-package (name)
  (or (find-package name) (err)))
          
(defun package-lock (&rest r)
		(declare (ignore r))
  )
  
(defun |(SETF PACKAGE-LOCK)| (&rest r)
		(declare (ignore r))
  )
  
(defun add-backquote (skel)
  (list 'BACKQUOTE skel))
  	       
(defun add-unquote (skel)
  (list 'UNQUOTE skel))
  
  
#|!!!

(in-package "EXT")
(export '(fcase))
(in-package "SYSTEM")



(defmacro fcase (test keyform &body clauses)
  (case-expand 'fcase test keyform clauses))
|#  
  

(in-package "CLOS")	

(defun typep-class (x c)
  (subclassp (class-of x) c))
  
  
(let ((clos-extra '(generic-flet generic-labels no-primary-method
                    class-prototype class-finalized-p finalize-inheritance)))
  ;; not in ANSI - export separately, after `re-export' above
  (export clos-extra "CLOS")
  ;; so that they are available in CL-USER even though it does not use CLOS
  (import clos-extra "EXT")
  (export clos-extra "EXT")) 

  
  

;(load "defseq")

(in-package "SYS")	


(PROGN

(sys::%putd 'defmacro
(sys::%putd 'sys::predefmacro ; predefmacro means "preliminary defmacro"
  (sys::make-macro
    (function defmacro (lambda (form env)
      (declare (ignore env))
      (let ((preliminaryp (eq (car form) 'sys::predefmacro)))
        (multiple-value-bind (expansion expansion-lambdabody name lambdalist docstring)
            (sys::make-macro-expansion (cdr form) form)
          (declare (ignore expansion-lambdabody lambdalist))
          `(LET ()
             (EVAL-WHEN ,(if preliminaryp '(LOAD EVAL) '(COMPILE LOAD EVAL))
               (SYSTEM::REMOVE-OLD-DEFINITIONS ',name
                 ,@(if preliminaryp '('T)))
               ,@(if docstring
                   `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                   '())
               (SYSTEM::%PUTD ',name
                 (SYSTEM::MAKE-MACRO ,(if preliminaryp
                                        `(SYSTEM::MAKE-PRELIMINARY ,expansion)
                                        expansion))))
             (EVAL-WHEN (EVAL)
               (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION
                             (CONS ',form (THE-ENVIRONMENT))))
             ',name))))))))

#-compiler
(predefmacro COMPILER::EVAL-WHEN-COMPILE (&body body) ; preliminary
  `(eval-when (compile) ,@body))

;; return 2 values: ordinary lambda list and reversed list of type declarations
(sys::%putd 'sys::specialized-lambda-list-to-ordinary
  (function sys::specialized-lambda-list-to-ordinary (lambda (spelalist caller)
    (multiple-value-bind (lalist speclist)
        #+clos (clos::decompose-specialized-lambda-list
                spelalist (clos::program-error-reporter caller))
        #-clos (copy-list spelalist) ; pacify "make check-recompile"
      (values
        lalist ; MAPCAN needs a LAMBDA which leads to infinite recursion
        (let ((decls '()))
          (block spelalist-to-ordinary
            (tagbody start
              (when (null lalist) (return-from spelalist-to-ordinary decls))
              (when (null speclist) (return-from spelalist-to-ordinary decls))
              (let ((arg (car lalist)) (spec (car speclist)))
                (if (not (eq spec 'T))
                  (setq decls (cons (list spec arg) decls))))
              (setq lalist (cdr lalist) speclist (cdr speclist))
              (go start)))))))))

(sys::%putd 'defun
(sys::%putd 'sys::predefun ; predefun means "preliminary defun"
  (sys::make-macro
    (function defun (lambda (form env)
      (if (atom (cdr form))
        (error-of-type 'source-program-error
          :form form
          :detail (cdr form)
          (TEXT "~S: cannot define a function from that: ~S")
          (car form) (cdr form)))
      (unless (function-name-p (cadr form))
        (error-of-type 'source-program-error
          :form form
          :detail (cadr form)
          (TEXT "~S: the name of a function must be a symbol, not ~S")
          (car form) (cadr form)))
      (if (atom (cddr form))
        (error-of-type 'source-program-error
          :form form
          :detail (cddr form)
          (TEXT "~S: function ~S is missing a lambda list")
          (car form) (cadr form)))
      (let ((preliminaryp (eq (car form) 'sys::predefun))
            (name (cadr form))
            (lambdalist (caddr form))
            (body (cdddr form)))
        (multiple-value-bind (body-rest declarations docstring)
            (sys::parse-body body t)
          (when *defun-accept-specialized-lambda-list*
            (multiple-value-bind (lalist decl-list)
                (sys::specialized-lambda-list-to-ordinary lambdalist (car form))
              (setq lambdalist lalist
                    declarations (nreconc decl-list declarations))))
          (let ((symbolform
                 (if (atom name)
                   `',name
                   `(LOAD-TIME-VALUE (GET-SETF-SYMBOL ',(second name)))))
                (lambdabody `(,lambdalist
                              ,@(if docstring `(,docstring) '())
                              (DECLARE (SYS::IN-DEFUN ,name) ,@declarations)
                              (BLOCK ,(function-block-name name)
                                ,@body-rest))))
            `(LET ()
               (SYSTEM::REMOVE-OLD-DEFINITIONS ,symbolform
                 ,@(if preliminaryp '('T)))
               ,@(if ; Is name declared inline?
                  (if (and compiler::*compiling*
                           compiler::*compiling-from-file*)
                    (member name compiler::*inline-functions* :test #'equal)
                    (eq (get (get-funname-symbol name) 'inlinable) 'inline))
                  ;; Is the lexical environment the top-level environment?
                  ;; If yes, save the lambdabody for inline compilation.
                  (if compiler::*compiling*
                    (if (and (null compiler::*venv*)
                             (null compiler::*fenv*)
                             (null compiler::*benv*)
                             (null compiler::*genv*)
                             (eql compiler::*denv* *toplevel-denv*))
                      `((COMPILER::EVAL-WHEN-COMPILE
                         (COMPILER::C-DEFUN
                          ',name (lambda-list-to-signature ',lambdalist)
                          ',lambdabody))
                        (EVAL-WHEN (LOAD)
                          (SYSTEM::%PUT ,symbolform 'SYSTEM::INLINE-EXPANSION
                                        ',lambdabody)))
                      `((COMPILER::EVAL-WHEN-COMPILE
                         (COMPILER::C-DEFUN
                          ',name (lambda-list-to-signature ',lambdalist)))))
                    (if (and (null (svref env 0))  ; venv
                             (null (svref env 1))) ; fenv
                       `((EVAL-WHEN (EVAL)
                           (LET ((%ENV (THE-ENVIRONMENT)))
                             (IF (AND (NULL (SVREF %ENV 0)) ; venv
                                      (NULL (SVREF %ENV 1)) ; fenv
                                      (NULL (SVREF %ENV 2)) ; benv
                                      (NULL (SVREF %ENV 3)) ; genv
                                      (EQL (SVREF %ENV 4) *TOPLEVEL-DENV*)) ; denv
                               (SYSTEM::%PUT ,symbolform
                                             'SYSTEM::INLINE-EXPANSION
                                             ',lambdabody)))))
                       '()))
                  `((COMPILER::EVAL-WHEN-COMPILE
                     (COMPILER::C-DEFUN
                      ',name (lambda-list-to-signature ',lambdalist)))))
               ,@(if docstring
                   `((SYSTEM::%SET-DOCUMENTATION ,symbolform
                                                 'FUNCTION ',docstring))
                   '())
               (SYSTEM::%PUTD ,symbolform
                 ,(if preliminaryp
                    `(SYSTEM::MAKE-PRELIMINARY (FUNCTION ,name (LAMBDA ,@lambdabody)))
                    `(FUNCTION ,name (LAMBDA ,@lambdabody))))
               (EVAL-WHEN (EVAL)
                 (SYSTEM::%PUT ,symbolform 'SYSTEM::DEFINITION
                               (CONS ',form (THE-ENVIRONMENT))))
               ',name)))))))))

(VALUES) )

(defun make-logical-pathname (&rest r)
  (apply #'make-pathname :logical t r))


(load "macros1")
(load "macros2")
(load "defs1")
(LOAD "lambdalist")             ; parsing ordinary lambda lists

(defun _to-keyword (s)
  (intern (symbol-name s) *keyword-package*))  


(remove-old-definitions 'push)
(remove-old-definitions 'incf)
(remove-old-definitions 'setf)


(LOAD "places")

(defun vector-push (x v)
  (let ((fp (fill-pointer v)))
    (unless (eql fp (array-total-size v))
      (setf (aref v fp) x)
      (setf (fill-pointer v) (1+ fp))
      fp)))

(defun vector-pop (v)
  (aref v (decf (fill-pointer v))))

#|!!!P
(defun (setf get) (v s i)
  (setf (getf (symbol-plist s) i) v))
  |#
  
(defun %set-documentation (symbol doctype value)
  (if value
  	(setf (getf (gethash symbol *documentation*) doctype) value)
    (multiple-value-bind (rec found-p) (gethash symbol *documentation*)
      (when (and found-p (remf rec doctype) (null rec))
        (remhash symbol *documentation*)))))



(LOAD "defpackage")

(defun bytep (b)
  (listp b))

(defun random-state-p (x)		; redefined later in (defstruct random-state...
  (typep x 'random-state))

(load "type")



;(deftype simple-string (&optional size)
;  `(simple-array string-char (,size)))

(load "byte")


;(when (memq :NO-HUGE *features*)
;  (pushnew :NO-CLOS *features*)
;  (pushnew :NO-COMPILER *features*))



(defun _read-sequence (fun seq stm start end)
  (setq end (_end end seq))
  (do* ((td (_get-valid-seq-type seq))
        (aset (_seq-access_set td))
        (u (_seq-upd td))
        (p (funcall (_seq-init_start td) seq start) (funcall u seq p))
        (i start (1+ i)))
          ((>= i end) i)
    (let ((a (funcall fun stm nil nil)))   
      (if a (funcall aset seq p a)
            (return i)))))

(defun read-char-sequence (seq stm &key (start 0) end)
  (funcall #'_read-sequence #'read-char seq stm start end))
  
(defun read-byte-sequence (seq stm &key (start 0) end)
  (funcall #'_read-sequence #'read-byte seq stm start end))

(defun write-char-sequence (seq stm &key (start 0) end)
  (write-string (coerce (subseq seq start end) 'string) stm)
  seq)
    
(defun make-weak-list (x)
  (let ((wl (%make-structure '(weak-list) 2)))
    (%record-store wl 1 (mapcar #'make-weak-pointer x))
    wl))
    
(defun weak-list-p (x)
  (%structure-type-p 'weak-list x))

(defun weak-list-list (wl)
  (unless (weak-list-p wl) (err))   
	(mapcan #'(lambda (x)
							(multiple-value-bind (v p) (weak-pointer-value x)
								(if p (list v))))
						(%record-ref wl 1)))
						
(defun (setf weak-list-list) (v wl)
  (unless (weak-list-p wl) (err))
  (%record-store wl 1 (mapcar #'make-weak-pointer v))
	v)

;----------------------------------	  

(unless (memq :NO-CLOS *features*)
  (load "uclos")   
  
  
  (LOAD "clos-package")           ; Early CLOS
	(LOAD "clos-macros")
	(LOAD "clos-class0")
	(LOAD "clos-metaobject1")
	(LOAD "clos-slotdef1")
	(LOAD "clos-stablehash1")
	(LOAD "clos-specializer1")
	(LOAD "clos-class1")
	(LOAD "clos-class2")
	(LOAD "clos-class3") ;!!! Patched
	
	(LOAD "defstruct")              ; DEFSTRUCT-macro
	(LOAD "format")                 ; FORMAT
	(load "international")	
	(LOAD "functions")              ; function objects
;	(LOAD "trace")                  ; TRACE
	(load "cmacros")
  
  
	#-LOAD_COMPILER_FAS
	(unless (memq :NO-COMPILER *features*)
		(load "compiler"))

	#+LOAD_COMPILER_FAS
	(progn 
		(load (merge-pathnames "compiler.fas" (car *args*)))
		(setq *features* (remove :LOAD_COMPILER_FAS *features*))
		
	)  

	(LOAD "defs2")                  ; CLtL2-definitions, optional
;	(LOAD "loop")                   ; CLtL2/ANSI-CL-LOOP, optional
	(LOAD "clos")                   ; CLOS
)		

(load "backquote") ;!!!

(defstruct (random-state (:constructor _make-random-state))
  data)
		

(when (memq :NO-HUGE *features*)
  (defun warn-of-type (type format-string &rest args)
		(declare (ignore format-string args))
    (warn type)))



(unless (memq :NO-HUGE *features*)
	(LOAD "loop")                   ; CLtL2/ANSI-CL-LOOP, optional

	(fmakunbound 'close)
	(fmakunbound 'stream-element-type)
	(LOAD "gray")                   ; STREAM-DEFINITION-BY-USER:GENERIC-FUNCTIONS
	
               ; FILL-OUTPUT-STREAM (for condition & describe)
	(LOAD "disassem"))               ; Disassembler


(LOAD "fill-out")				; required by "condition"
(LOAD "condition")              ; Conditions

(define-condition stack-overflow-error (control-error) ())
(defvar *stack-overflow-instance* (make-condition 'ext::stack-overflow-error))	; // to make nex create simpler

(defun invoke-debugger (c)
  (if *debugger-hook*
    (let ((h *debugger-hook*)
          (*debugger-hook* nil))         
      (funcall h c h)))
  (funcall *break-driver* nil c t)
  (unwind-to-driver 1))


(unless (memq :NO-HUGE *features*)      ;  shoul be separate from previos (progn (load...  to use right symbols
	(load "loadform"))               ; `make-load-form'

(defun delta4 (n1 n2 o1 o2 shift)
  (- (+ (ash n1 shift) n2) (+ (ash o1 shift) o2)))

#|!!! implemented in CLISP
	(defmacro time (form)
		(let ((t-real (gensym))
					(t-run (gensym))
					(t-gccount (gensym)))
			`(let ((,t-real (get-internal-real-time))
						 (,t-run (get-internal-run-time))
						 (,t-gccount (_gc-count)))
				 (unwind-protect ,form
					 (let ((real (- (get-internal-real-time) ,t-real))
								 (run (- (get-internal-run-time) ,t-run))
								 (gccount (- (_gc-count) ,t-gccount)))
						 (terpri *trace-output*)
						 (write-string "Real time: " *trace-output*)
						 (write (float (/ real internal-time-units-per-second)) :stream *trace-output*)
						 (write-string " sec." *trace-output*)
						 (terpri *trace-output*)
						 (write-string "Run time: " *trace-output*)
						 (write (float (/ run internal-time-units-per-second)) :stream *trace-output*)
						 (write-string " sec." *trace-output*)
						 (terpri *trace-output*)
						 (when (plusp gccount)
							 (terpri *trace-output*)
							 (write-string "GC: " *trace-output*) (write gccount :stream *trace-output*)
							 (terpri *trace-output*)))))))
|#
	


(define-condition simple-condition ()
  (($format-control :initarg :format-control :initform nil
                    :reader simple-condition-format-string ; for CLtL2 backward compatibility
                    :reader simple-condition-format-control)
   ($format-arguments :initarg :format-arguments :initform nil
                      :reader simple-condition-format-arguments)
   ($hresult :initarg :hresult :initform nil
                      :reader simple-condition-hresult))
)


(clos:defmethod print-object :around ((condition simple-condition) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (let ((fstring (simple-condition-format-control condition)))
      (if fstring
        (progn
			(let ((hr (sys::simple-condition-hresult condition)))
				(if hr
					(format stream " fatal error L~D: " (logand 65535 hr))))
        	(apply #'format stream fstring (simple-condition-format-arguments condition)))
        (clos:call-next-method))))
  condition)

(defmethod print-object ((c type-error) stm)
 	(if (or *print-escape* *print-readably*)
	  (clos:call-next-method)
	  (format stm "~A is not a ~A" (type-error-datum c) (type-error-expected-type c)))
	c)


(defmethod print-object ((c stream-error) stm)
 	(if (or *print-escape* *print-readably*)
	  (clos:call-next-method)
	  (let* ((stm (stream-error-stream c))
             (lpos (sys::line-position stm)))
		(format *error-output* "~A(~A~@[,~D~]): fatal error L~D: " (namestring stm) (sys::line-number stm) lpos
                                             (logand 65535 (simple-condition-hresult c)) )
		(pretty-print-condition cnd *error-output*)))
	c)


	(defun report-error (condition)
  		(when *report-error-print-backtrace*
    		(print-backtrace :out *error-output*))
	  	(fresh-line *error-output*)
        (write-string "*** - " *error-output*) (pretty-print-condition condition *error-output*)
		(elastic-newline *error-output*))
	
	
	(defun exitunconditionally (condition) ; ABI
		(if (boundp 'compiler::*error-count*)
		  (incf compiler::*error-count*))
  		(report-error condition)
  		(exit (if (typep condition 'simple-condition)
                 (or (simple-condition-hresult condition) t)
                 t)))                     ; exit Lisp with error




(defun coerce-to-condition-ex (type hresult datum arguments &rest more-initargs)
	(let ((cnd (handler-case
					(apply #'make-condition type  :hresult hresult :format-control datum :format-arguments arguments more-initargs)
			    (TYPE-ERROR (error) (error error))
			    (ERROR (error)
		      ;; ANSI CL wants a type error here.
		      (error-of-type 'type-error :datum (cons datum arguments)  :expected-type '(satisfies valid-condition-designator-p) "~A" error)))))


		(let ((*print-escape* nil)		;!!!?
			  (*print-readably* nil))
		  (signal cnd)
		  (invoke-debugger cnd))))

(defun correctable-error-ex (options type hresult datum arguments &rest more-initargs)
	(let ((cnd (handler-case
					(apply #'make-condition type  :hresult hresult :format-control datum :format-arguments arguments more-initargs)
			    (TYPE-ERROR (error) (error error))
			    (ERROR (error)
		      ;; ANSI CL wants a type error here.
		      (error-of-type 'type-error :datum (cons datum arguments)  :expected-type '(satisfies valid-condition-designator-p) "~A" error)))))
      (correctable-error cnd options)))


 (defun cerror-of-type (continue-format-string type &rest arguments)
   (let ((keyword-arguments '()))
     (loop
       (unless (and (consp arguments) (symbolp (car arguments))) (return))
       (push (pop arguments) keyword-arguments)
       (push (pop arguments) keyword-arguments))
     (setq keyword-arguments (nreverse keyword-arguments))
     (let ((error-format-string (first arguments))
           (args (rest arguments)))
       (apply #'cerror
         continue-format-string
         (if (or *error-handler* (not *use-clcs*))
           error-format-string
           (apply #'coerce-to-condition error-format-string args
                  'cerror (convert-simple-condition type) keyword-arguments))
         args))))





(load "print")




#+CLISP-DEBUG
(defun compiler::c-style-warn (&rest r)
	(princ "."));!!!
  
  
  
;;; redefine "defs1", remove #+CLISP
(defun require (module-name &optional (pathname nil p-given))
  (setq module-name (module-name module-name))
  (unless (member module-name *modules* :test #'string=)
    (unless p-given (setq pathname (pathname module-name)))
    (let (#+CLISP (*load-paths* *load-paths*))
      (pushnew (merge-pathnames "dynmod/" *lib-directory*) *load-paths*
                       :test #'equal)
      (when *load-truename*
                (pushnew (make-pathname :name nil :type nil
                                        :defaults *load-truename*)
                         *load-paths* :test #'equal))
      (if (atom pathname) (load pathname) (mapcar #'load pathname)))))



(unless (memq :NO-HUGE *features*)
  

	(defstruct (load-time
							 (:print-function (lambda (x s k)
													(declare (ignore k))
																	(write-string "#." s)
																	(write (load-time-obj x) :stream s))))
		obj)

		#|  
			
	(defun _print-load-time (x s)  
		(write-string "#." s)
		(write (load-time-obj x) :stream s)
		x)

	(defmethod print-object ((x load-time) s)
		(_print-load-time x s))
		|#

	(defun make-load-time-eval (x)
		(make-load-time :obj x))

	(load "pprint")		

	(defun pprint (x &optional stm)
		(write x :stream stm :escape t :pretty t))

	(setq *print-pretty* t)  
	
		
	(load "describe")  
;!!!	(load "inspect")

	(definternational date-format
		(t ENGLISH))
	(deflocalized date-format ENGLISH
		(formatter
		 "~1{~5@*~D-~4@*~2,'0D-~3@*~2,'0D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"))
	(defun date-format ()
		(localized 'date-format))



	;;; redefine "defs1"

	(defun encode-universal-time (second minute hour date month year &optional tz)
       (when (<= 0 year 99)
         (incf year (* 100 (ceiling (- (nth-value 5 (get-decoded-time)) year 50) 100))))
		(_encode-universal-time second  minute hour date month year tz))

	(defun decode-universal-time (ut &optional tz)
      (_decode-universal-time ut tz))
			
	(defun get-decoded-time ()
		(decode-universal-time (get-universal-time)))

	 

		;(dolist (x '(expand-loop formatter-main-1 format-parse-cs))
		;  (_compile x))
				
;!!!	(clos::u-def-unbound
;!!!		(sys::%record-ref (clos::allocate-std-instance clos::<standard-object> 3) 2))

;!!!	(load "condition")

			

		
	;(in-package "SYS")	



	(load "reploop")
	
	(defun commands0 ()  ;; To override string with "CLISP"
		(list*
		 (TEXT "
	Help (abbreviated :h) = this list
	Use the usual editing capabilities.
	\(quit) or (exit) leaves LISP.")

		 (cons "Help"         #'debug-help)
		 (cons ":h"           #'debug-help)
		 (wrap-user-commands *user-commands*)))

	(load "runprog")

)   ; :NO-HUGE

(setq *use-clcs* t)	

