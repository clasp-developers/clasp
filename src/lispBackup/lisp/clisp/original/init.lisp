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

(SYS::_load "export")

(eval-when (compile load eval)
  (setq *package* (find-package "SYS")))
  
(%putd 'caar 		(%make-closure 'caar 		#15Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 19 02) nil nil))
(%putd 'cadr 		(%make-closure 'cadr 		#15Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 19 02) nil nil))
(%putd 'cdar 		(%make-closure 'cdar 		#15Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 19 02) nil nil))
(%putd 'cddr 		(%make-closure 'cddr 		#15Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 19 02) nil nil))
(%putd 'caaar 	(%make-closure 'caaar 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5B 19 02) nil nil))
(%putd 'caadr 	(%make-closure 'caadr 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5B 19 02) nil nil))
(%putd 'cadar 	(%make-closure 'cadar 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5B 19 02) nil nil))
(%putd 'caddr 	(%make-closure 'caddr 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5B 19 02) nil nil))
(%putd 'cdaar 	(%make-closure 'cdaar 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5C 19 02) nil nil))
(%putd 'cdadr 	(%make-closure 'cdadr 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5C 19 02) nil nil))
(%putd 'cddar 	(%make-closure 'cddar 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5C 19 02) nil nil))
(%putd 'cdddr 	(%make-closure 'cdddr 	#16Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5C 19 02) nil nil))
(%putd 'caaaar 	(%make-closure 'caaaar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5B 5B 19 02) nil nil))
(%putd 'caaadr 	(%make-closure 'caaadr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5B 5B 19 02) nil nil))
(%putd 'caadar 	(%make-closure 'caadar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5B 5B 19 02) nil nil))
(%putd 'caaddr 	(%make-closure 'caaddr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5B 5B 19 02) nil nil))
(%putd 'cadaar 	(%make-closure 'cadaar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5C 5B 19 02) nil nil))
(%putd 'cadadr 	(%make-closure 'cadadr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5C 5B 19 02) nil nil))
(%putd 'caddar 	(%make-closure 'caddar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5C 5B 19 02) nil nil))
(%putd 'cadddr 	(%make-closure 'cadddr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5C 5B 19 02) nil nil))
(%putd 'cdaaar 	(%make-closure 'cdaaar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5B 5C 19 02) nil nil))
(%putd 'cdaadr 	(%make-closure 'cdaadr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5B 5C 19 02) nil nil))
(%putd 'cdadar 	(%make-closure 'cdadar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5B 5C 19 02) nil nil))
(%putd 'cdaddr 	(%make-closure 'cdaddr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5B 5C 19 02) nil nil))
(%putd 'cddaar	(%make-closure 'cddaar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5B 5C 5C 19 02) nil nil))
(%putd 'cddadr 	(%make-closure 'cddadr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5B 5C 5C 19 02) nil nil))
(%putd 'cdddar 	(%make-closure 'cdddar #17Y(00 00 00 00 01 00 00 00 00 00 9E 5B 5C 5C 5C 19 02) nil nil))
(%putd 'cddddr 	(%make-closure 'cddddr #17Y(00 00 00 00 01 00 00 00 00 00 9E 5C 5C 5C 5C 19 02) nil nil))

(proclaim '(special *macroexpand-hook*))
(setq *macroexpand-hook* #'funcall)

(proclaim '(special compiler::*compiling* compiler::*compiling-from-file*
            compiler::*c-error-output*)) ; for load/compiling
(setq compiler::*compiling* nil)


#-COMPILER ; only for bootstrapping
(progn

;; preliminary, no expansion at GET_CLOSURE:
(sys::%putd '%expand-lambdabody-main
  (function %expand-lambdabody-main (lambda (lambdabody venv fenv)
    (declare (source nil) (ignore venv fenv))
    lambdabody)))

;; preliminary, defun is to be expanded trivially:
(sys::%putd 'defun
  (sys::make-macro
    (function defun (lambda (form env)
      (declare (ignore env))

      #| (let ((name (cadr form))
               (lambdalist (caddr form))
               (body (cdddr form)))
           `(SYS::%PUTD ',name (FUNCTION ,name (LAMBDA ,lambdalist ,@body))))
      |#
      (let ((name (cadr form)))
        (list 'sys::%putd (list 'quote name)
              (list 'function name (cons 'lambda (cddr form)))))))))

           
)

#-ULISP-BACKQUOTE-AS-SPECIAL-OPERATOR
(progn
	(%putd 'backquote (make-macro
		(function backquote (lambda (form env)
				(declare (ignore env))
			(bq-expand (cadr form))))))

	;; The BQ-NCONC form is a marker used in the backquote-generated code.
	;; It tells the optimizer that the enclosed forms may be combined with
	;; NCONC. By defining BQ-NCONC as a macro, we take care of any `surviving'
	;; occurences that are not removed and processed by the optimizer.
	(sys::%putd 'sys::bq-nconc
		(sys::make-macro
		 (function sys::bq-nconc (lambda (form env)
			 (declare (ignore env))
			 (if (cddr form)
					 `(nconc ,@(cdr form))
					 (cadr form))))))
)

#|!!!hardwired
(%putd 'proclaim
  (function proclaim (lambda (spec)
		(declare (source nil))
		(if (eq (car spec) 'special) (_proclaim-special (cdr spec))))))
|#


(%putd 'not (symbol-function 'null))
(%putd 'char-int (symbol-function 'char-code))
		
(%putd 'first (symbol-function 'car))
(%putd 'second (symbol-function 'cadr))
(%putd 'third (symbol-function 'caddr))
(%putd 'fourth (symbol-function 'cadddr))
(%putd 'rest (symbol-function 'cdr))

#-COMPILER
(sys::%putd 'sys::predefun
	(%putd 'defun (make-macro
		(function defun (lambda (form env)
			 (let ((name (cadr form)))
				 `(%putd ',name
										 (function ,name ,(cons 'lambda (cddr form))))))))))

#-COMPILER
(sys::%putd 'sys::predefun
	(%putd 'defun
	 (make-macro
		(function defun
		 (lambda (form env)
			(let* ((name (cadr form))
						 (bname (cond ((symbolp name) name)
													 ((_setf-name-p name) (cadr name))
													 (t (err))))
						 (fsym (if (symbolp name) name (get-setf-symbol bname))))
				`(PROGN
					(%PUTD ',fsym (_GET-CLOSURE ',(cddr form) ',name t))   ;!!! maybe need env too?
					',name)))))))

#|
                         
(%putd 'defun
 (make-macro
  (function defun
   (lambda (form env)
    (let* ((name (cadr form))
           (bname (cond ((symbolp name) name)
            						 ((_setf-name-p name) (cadr name))
            						 (t (err))))
           (fsym (if (symbolp name) name (get-setf-symbol bname))))
      (multiple-value-bind (body decls docs)
					(parse-body (cdddr form) t)
				`(PROGN
		        (when _pp (_pr "-------------------------")  
        		          (_pr '(LAMBDA ,(caddr form) ,(cons 'DECLARE decls) ,docs (BLOCK ,bname ,@body))))
				 (%PUTD ',fsym (FUNCTION ,name (LAMBDA ,(caddr form) ,(cons 'DECLARE decls) ,docs (BLOCK ,bname ,@body))))
				        ',name)))))))
				        
|#				        

(sys::%putd 'sys::remove-old-definitions
  (function sys::remove-old-definitions (lambda (symbol &optional (preliminary nil)) ; ABI
  ;; removes the old function-definitions of a symbol
				(declare (ignore preliminary))
  (when (special-operator-p symbol)
    (error-of-type 'error
      (TEXT "~S is a special operator and may not be redefined.")
      symbol))
      #|
  (unless preliminary
    (sys::check-redefinition symbol "DEFUN/DEFMACRO"
                             (sys::fbound-string symbol)))
                             |#
  (fmakunbound symbol) ; discard function & macro definition
  ;; Property sys::definition is not discarded, because it is
  ;; soon reset, anyway.
;!!!  (remprop symbol 'sys::macro) ; discard macro definition
;!!!  (remprop symbol 'sys::defstruct-reader) ; discard DEFSTRUCT information
;!!!  (sys::%set-documentation symbol 'FUNCTION nil)
  (when (get symbol 'sys::inline-expansion)
    (sys::%put symbol 'sys::inline-expansion t))
  (when (get symbol 'sys::traced-definition) ; discard Trace
    (warn (TEXT "DEFUN/DEFMACRO: redefining ~S; it was traced!")
          symbol)
    (untrace2 symbol)))))

(%proclaim-constant 'lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment))
  
(defun symbol-to-keyword (s)
	(intern (symbol-name s) *keyword-package*))  

(defun ext:string-concat (&rest seqs)
  (apply #'concatenate 'string seqs))

(_load "macro")

(defmacro return (&optional v)
  (list 'return-from nil v))       

  
(defun _func-of (fn)
  (symbol-function (if (_setf-name-p fn) (get-setf-symbol (cadr fn))
                                         fn)))

#|!!!  
(defun _make-macro-expander (macrodef)
  (make-macro (eval (_make-macro-expansion macrodef))))
|#


#|!!!

(defun _last2 (p q)
  (if (atom p) q
               (_last2 (cdr p) (cdr q))))

(defun _last (n p q)
  (if (atom p) q
               (if (plusp n) (_last (1- n) (cdr p) q)
                             (_last2 (cdr p) (cdr q)))))
|#


(defun nth (n a)
  (car (nthcdr n a)))

(defun fifth (a)
  (car (cddddr a)))

(defun sixth (a)
  (car (cdr (cddddr a))))
  
(defun seventh (a)
  (car (cddr (cddddr a))))

(defun eighth (a)
  (car (cdddr (cddddr a))))

(defun ninth (a)
  (car (cddddr (cddddr a))))

(defun tenth (a)
  (car (cdr (cddddr (cddddr a)))))

(defun function-macro-p (x)
  (eq (type-of x) 'function-macro))

(defun _check-numbers (r)
  (if r (if (numberp (car r)) (_check-numbers (cdr r))
                              (error 'type-error))))
                              

#|!!!R

(defun _reduce-list (f r init)
  (if r (_seq-infix f (car r) (cdr r))
        init))


(defun _seq-infix (f a r)
  (if r (_seq-infix f (apply f (multiple-value-list (_norm-pair a (car r)))) (cdr r))
        a))

(defun _reduce-list-ex (f a r init)
  (if r (_seq-infix f a r)
        (apply f (multiple-value-list (_norm-pair init a)))))
        
|#
    
(defun _to-boolean (x)
	(if x t))

	
(defun make-preliminary (closure) ; ABI
  (sys::\(setf\ closure-name\) (list 'ext::preliminary (sys::closure-name closure))
                                 closure)
  closure)
	

(%putd 'fasthash-eq #'eq)	
(%putd 'stablehash-eq #'eq)	
(%putd 'fasthash-eql #'eql)	
(%putd 'stablehash-eql #'eql)	
(%putd 'fasthash-equal #'equal)	
(%putd 'stablehash-equal #'equal)
 
(_load "explambda")  


;;; Here we have Expander and may reload already loaded code, expanding it for speed

(_load "macro")
(_load "explambda")

(defmacro in-package (name)
  `(eval-when (compile load eval)
     (setq *package* (sys::_get-package ',name))))
     
(in-package "SYSTEM")


(defun fixnump (x)
  (_to-boolean (memq (type-of x) '(bit fixnum))))

(defun weak-pointer-p (x)
  (eq (type-of x) 'weak-pointer))
 
#|!!!R hardwired  because can be called from GF dispatch
(defun stringp (x)
  (let ((a (type-of x)))
    (and (consp a)
         (_to-boolean (memq (car a) '(string base-string simple-string simple-base-string))))))

(defun pathnamep (x)
  (eq (type-of x) 'pathname))

|#
  
(defun simple-string-p (x)
  (let ((a (type-of x)))
    (and (consp a)
         (_to-boolean (memq (car a) '(simple-string simple-base-string))))))

(defun bit-vector-p (x)
  (let ((a (type-of x)))
    (and (consp a)
         (_to-boolean (memq (car a) '(bit-vector simple-bit-vector))))))

(defun packagep (x)
  (eq (type-of x) 'package))

(defun readtablep (x)
  (eq (type-of x) 'readtable))

(defun hash-table-p (x)
  (eq (type-of x) 'hash-table))


(defmacro declaim (&body r)
   (if r `(progn (proclaim ',(car r))
                 (declaim ,@(cdr r)))))

(defmacro dolist ((p x &optional r) &body body)
  (let ((n (gensym)))
    `(do* ((,n ,x (cdr ,n))
           (,p (car ,n) (car ,n)))
         ((null ,n) ,r)
       ,@body)))

(defmacro dotimes ((p x &optional r) &body body)
  (let ((n (gensym)))
    `(do ((,p 0 (1+ ,p))
          (,n ,x))
         ((>= ,p ,n) ,r)
       ,@body)))

(defun simple-bit-vector-p (x)
  (values (subtypep (type-of x) 'simple-bit-vector)))

                                              
(defun typep (x t1 &optional e)
		(declare (ignore e))
  (let ((tt (type-of t1)))
    (cond ((eq tt 'symbol)
             (if (eq t1 'atom) (atom x)
                                             (subtypep (type-of x) t1)))
          ((eq tt 'cons) (let ((c (car t1)))
                           (cond ((eq c 'or) (member-if (lambda (y) (typep x y)) (cdr t1)))
                                 ((eq c 'and) (not (member-if-not (lambda (y) (typep x y)) (cdr t1))))
                                 ((eq c 'not) (not (typep x (cadr t1))))
                                 ((eq c 'member) (member-if (lambda (y) (eql x y)) (cdr t1)))
                                 ((err))))))))

(defun get-setf-expansion (p &optional env)
  (block nil
		(tagbody
			lab
			(if (consp p) (let ((f (get (car p) 'SETF-EXPANDER)))
												(if f (return-from get-setf-expansion
												        (if (symbolp f)
																	(do* ((storevar (gensym))
																				(tempvars nil (cons (gensym) tempvars))
																				(tempforms nil (cons (car formr) tempforms))
																				(formr (cdr p) (cdr formr)))
																			 ((atom formr)
																				(setq tempforms (nreverse tempforms))
																				(values tempvars
																								tempforms
																								`(,storevar)
																								`(,f ,@tempvars ,storevar)
																								`(,(car p) ,@tempvars)))
																		(when (eq f 'set-slot-value)						
																		;!!!  (_pr "---------------")
																		)
																	)
												          (apply f (cdr p)))))))
			(if (eq p (setq p (macroexpand-1 p env)))
				(return))
			(go lab)))
  (cond ((symbolp p) (let ((ta (gensym)))
                       (values nil nil (list ta) (list 'setq p ta) p)))
        ((and (consp p) (symbolp (car p)))
					(let* ((a (car p))
								(d (cdr p))
								(f (get a 'SETF-EXPANDER)))
						(if f (apply f d)
									(let ((sf (_subforms d))
												(ta (gensym)))
										(values sf d (list ta) (list* (list 'setf a) ta sf) (cons a sf))))))
        (t (error-of-type 'source-program-error
             (TEXT "~S: Argument ~S is not a SETF place.")
             'get-setf-expansion p))))
             
(defun get-setf-method (form &optional (env (vector nil nil)))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion form env)
    (unless (and (consp stores) (null (cdr stores)))
      (error-of-type 'source-program-error
        (TEXT "SETF place ~S produces more than one store variable.")
        form
    ) )
    (values vars vals stores store-form access-form)
) )
             
(defmacro pop (p)
  (multiple-value-bind (x v s w a) (get-setf-expansion p)
    (let ((n (car s)))
      `(let* ,(mapcar #'list 
                      (append x s)
                      (append v (list a)))
         (prog1 (car ,n)
                (setq ,n (cdr ,n))
                ,w)))))

(defun err ()
  (error "Unspecified error"))              
                

#|!!!Hardcoded, should generate type-error in not proper=list
(defun values-list (r)
  (apply #'values r))
|#

(defun macroexpand (f &optional env)
  (multiple-value-bind (r p) (macroexpand-1 f env)
    (if p (values (macroexpand r env) t)
          (values f nil))))                    		

(defun _callf2 (func n place args)
  (let ((z (gensym)))
    (multiple-value-bind (x v s w a) (get-setf-expansion place)
      (list 'let* (mapcar #'list
                      (cons z (append x s))
                      (cons n (append v (list (list* func z a args)))))
         w))))
         
(defmacro pushnew (x p &rest rest &key key test test-not)
		(declare (ignore key test test-not))
  (_callf2 'adjoin x p rest))         
         
(defun _argnames (lam)
	(if lam (let ((a (car lam))
								(d (_argnames (cdr lam))))
	   				(cond ((_lambda-list-keyword-p a) d)
	   							((symbolp a) (cons a d))
	   							((consp a) (cons (car a) d))
	   							((err)))))) ;!!! need process &key &aux         
	   							
(defun _callf (func place &rest args)
  (multiple-value-bind (x v s w a) (get-setf-expansion place)
    `(let* ,(mapcar #'list
                    (append x s)
                    (append v (list `(,func ,a ,@args))))
       ,w)))

(defmacro define-modify-macro (name lam fun &optional doc)
	(let ((p (gensym)))
	 	`(DEFMACRO ,name (,p ,@lam) ,doc	 	
			(_callf ',fun ,p ,@(_argnames lam)))))
																										
(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)


#|                                                                                                        
(defmacro incf (p &optional (delta 1))
  (_callf #'+ p (list delta)))

(defmacro decf (p &optional (delta 1))
  (_callf #'- p (list delta)))

|#                                


(defun _setf-expansions (p)
  (if p (multiple-value-bind (x v s w a) (get-setf-expansion (car p))
          (cons (list x v s w a) (_setf-expansions (cdr p))))))

(defmacro push (item place)
   (_callf2 'cons item place nil))

(defun _psetf (r forms env)
  (if r (multiple-value-bind (x v s w a) (get-setf-expansion (car r) env)
				(declare (ignore a))
          (let ((sf (_psetf (cddr r) (append forms (list w)) env)))
            `(let* ,(mapcar #'list x v)
               ,(if (> (length s) 1) (list 'multiple-value-bind s (cadr r) sf)
                                     (list 'let (list (list (car s) (cadr r))) sf )))))
        `(progn ,@forms
                nil)))

(defmacro psetf (&body r &environment env)
  (_psetf r nil env))
                                 
(defun get-setf-symbol (symbol &optional env)
		(declare (ignore env))
  (or (get symbol 'SETF-FUNCTION)
      (progn
        (when (get symbol 'SETF-EXPANDER)
          (warn (ENGLISH "The function (~S ~S) is hidden by a SETF expander.")
                'setf symbol
        ))
        (%put symbol 'SETF-FUNCTION (setf-symbol symbol)))))
        
(defun (setf nth) (v n a)
  (setf (car (nthcdr n a)) v))

(defun (setf first) (n a)
  (setf (car a) n))

(defun (setf second) (n a)
  (setf (car (cdr a)) n))     
  
(defun (setf third) (n a)
  (setf (car (cddr a)) n))
  
(defun (setf fourth) (n a)
  (setf (car (cdddr a)) n))
  
(defun (setf fifth) (n a)
  (setf (car (cddddr a)) n))

(defun (setf sixth) (n a)
  (setf (car (cdr (cddddr a))) n))

(defun (setf seventh) (n a)
  (setf (car (cddr (cddddr a))) n))

(defun (setf eighth) (n a)
  (setf (car (cdddr (cddddr a))) n))

(defun (setf ninth) (n a)
  (setf (car (cddddr (cddddr a))) n))

(defun (setf tenth) (n a)
  (setf (car (cdr (cddddr (cddddr a)))) n))

(defun (setf rest) (n a)
  (setf (cdr a) n))

(defun _supertypes (x)
  (cons x (cond ((memq x '(symbol sequence character stream function array readtable package
                            pathname hash-table number))
                   '(t))
                ((eq x t)                   nil)
                ((eq x 'keyword)            '(symbol t))
                ((eq x 'boolean)            '(symbol t))
                ((memq x '(fixnum bignum)) (_supertypes 'integer))
                ((memq x '(compiled-function function)) '(function t))
                ((memq x '(standard-char base-char extended-char)) '(character t))
                ((eq x 'integer)            '(rational real number t))
                ((eq x 'signed-byte)        (_supertypes 'integer))
                ((eq x 'unsigned-byte)      (_supertypes 'signed-byte))
                ((eq x 'bit)                (cons 'fixnum (_supertypes 'unsigned-byte)))
                ((eq x 'ratio)              '(rational real number t))
                ((eq x 'list)               '(sequence t))
                ((eq x 'float)              '(real number t))
                ((memq x '(short-float single-float double-float long-float)) (_supertypes 'float))
                ((eq x 'complex)            '(number t))
                ((eq x 'cons)               (_supertypes 'list))
                ((eq x 'null)               '(symbol list sequence t))
                ((eq x 'vector)             '(array sequence t))
                ((eq x 'simple-array)       '(array t))
                ((eq x 'simple-vector)      `(simple-array ,@(_supertypes 'vector)))
                ((eq x 'string)             (_supertypes 'vector))
                ((eq x 'base-string)        (_supertypes 'string))
                ((eq x 'simple-string)      '(string vector simple-array array sequence t))
                ((eq x 'simple-base-string) `(base-string ,@(_supertypes 'simple-string)))
                ((eq x 'bit-vector)         '(_supertypes 'vector))
                ((eq x 'simple-bit-vector)  '(bit-vector vector simple-array array sequence t))
				((eq x 'logical-pathname)  '(pathname))
                (t (mapcar #'class-name (class-precedence-list (find-class x)))))))

(defun subtypep (t1 t2 &optional e)
		(declare (ignore e))
  (if (eq (type-of t2) 'cons) (values t t)
                              (if t1 (values (memq t2 (_supertypes t1)) t)
                                     (values t t))))


(defun %set-documentation (symbol doctype value) ; !!! dummy
		(declare (ignore symbol doctype value))
  )                               

#|!!!P
(defmacro define-setf-expander (af lam &body body &environment env)
	(multiple-value-bind (forms decls doc)
			(parse-body body t)
		`(PROGN (%PUT ',af 'SETF-EXPANDER #'(lambda ,lam ,@forms))
						(%SET-DOCUMENTATION ',af 'SETF ,doc)
						',af)))
								
(define-setf-expander values (&rest p)
  (let ((r (_setf-expansions p)))
    (values (apply #'append (mapcar #'first r))
            (apply #'append (mapcar #'second r))
            (apply #'append (mapcar #'third r))
            `(values ,@(mapcar #'fourth r))
            `(values ,@(mapcar #'fifth r)))))
|#						

								

(defun _get-defsetf-store (xs vars forms)
	`(LET ,(mapcar #'(lambda (x y) (list x (list 'quote y))) vars xs)
		,@forms))

;!!!		
(defmacro defsetf (af uf &body body)
	(if (listp uf)
		(let* ((lam uf)
					 (ss (car body))
					 (forms (cdr body))
					 (n (length lam))
					 (args (_gensyms n)))
			`(EVAL-WHEN (LOAD COMPILE EVAL)
				(DEFINE-SETF-EXPANDER ,af ,args
					(LET ((x (_gensyms ,n))
								(s (_gensyms ,(length ss))))
						(VALUES
							x
							(LIST ,@args)
							s
							(eval (_get-defsetf-store (append x s) ',(append lam ss) ',forms))
							(cons ',af x))))
				',af))
		`(EVAL-WHEN (LOAD COMPILE EVAL)
      (%PUT ',af 'SETF-EXPANDER ',uf) ;!!! need also doc
			',af)))
		
#|				
		(let ((store (car body))
					(forms (cdr body)))
			`(DEFINE-SETF-EXPANDER ,af (&rest r)
				(let ((x (_subforms r))
							(s (gensym)))
					(values x r (list s) (cons ',uf (append x (list s))) (cons ',af x)))))))
|#					
							
(defsetf symbol-function %putd)
          
(defmacro _def_c_r (n f r)
	`(defun (setf ,n) (x p) (setf (,f (,r p)) x)))

(_def_c_r caar car car)
(_def_c_r cadr car cdr)
(_def_c_r cdar cdr car)
(_def_c_r cddr cdr cdr)
(_def_c_r caaar car caar)
(_def_c_r caadr car cadr)
(_def_c_r cadar car cdar)
(_def_c_r caddr car cddr)
(_def_c_r cdaar cdr caar)
(_def_c_r cdadr cdr cadr)
(_def_c_r cddar cdr cdar)
(_def_c_r cdddr cdr cddr)
(_def_c_r caaaar car caaar)
(_def_c_r caaadr car caadr)
(_def_c_r caadar car cadar)
(_def_c_r caaddr car caddr)
(_def_c_r cadaar car cdaar)
(_def_c_r cadadr car cdadr)
(_def_c_r caddar car cddar)
(_def_c_r cadddr car cdddr)
(_def_c_r cdaaar cdr caaar)
(_def_c_r cdaadr cdr caadr)
(_def_c_r cdadar cdr cadar)
(_def_c_r cdaddr cdr caddr)
(_def_c_r cddaar cdr cdaar)
(_def_c_r cddadr cdr cdadr)
(_def_c_r cdddar cdr cddar)
(_def_c_r cddddr cdr cdddr)
						
							
(defsetf car %rplaca)

(defsetf cdr %rplacd)

(defsetf symbol-value set)


(defsetf macro-function _setf_macro-function)

(defsetf weak-pointer-value _setf_weak-pointer-value)

(defun _setf (r env)
  (if r (cons (multiple-value-bind (x v s w a) (get-setf-expansion (car r) env)
				(declare (ignore a))
                `(let* ,(mapcar #'list x v)
                   ,(if (cdr s) (list 'multiple-value-bind s (cadr r) w)
                                (list 'let (list (list (car s) (cadr r))) w))))
              (_setf (cddr r) env))))
              
(defmacro setf (&body r &environment env)
  (let ((z (_setf r env)))
    (if (cdr z) (cons 'progn z)
                (car z))))                                            

#|P
(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method place env)
    (let* ((storevar (gensym))
           (indicatorvar (gensym))
           (defaultvar-list (if default (list (gensym)) `()))
          )
      (values
        `(,@temps    ,indicatorvar ,@defaultvar-list)
        `(,@subforms ,indicator    ,@(if default `(,default) `()))
        `(,storevar)
        `(LET ((,(first stores) (SYS::%PUTF ,getterform ,indicatorvar ,storevar)))
           ,@defaultvar-list ; defaultvar zum Schein auswerten
           (WHEN ,(first stores) ,setterform)
           ,storevar
         )
        `(GETF ,getterform ,indicatorvar ,@defaultvar-list)
) ) ) )
|#
          
#|!!!

(defun _setf_getf1 (p i)
  (if p (if (eq i (car p)) (cdr p)
                           (_setf_getf1 (cddr p) i))))       

(defun _setf_getf (p i v)
  (let ((x (_setf_getf1 p i)))
    (if x (progn (rplaca x v)
                 p)
          (list* i v p))))

(define-setf-expander getf (p i &optional d)
  (multiple-value-bind (temps vals stores store-form access-form) (get-setf-expansion p)
     (let ((btemp (gensym))     ;Temp var for byte specifier.
           (store (gensym))     ;Temp var for byte to store.
           (stemp (car stores))) ;Temp var for int to store.
       (if (cdr stores) (error "Can't expand this."))
       (values (append temps (list btemp))       ;Temporary variables.
               (append vals (list i))     ;Value forms.
               (list store)             ;Store variables.
               `(let ((,stemp (_setf_getf ,access-form ,btemp ,store )))
                  ,store-form
                  ,store)               ;Storing form.
               `(getf ,access-form ,btemp) ;Accessing form.
              ))))
|#              
              
(defmacro defvar (n &optional (i nil i-p) (d nil d-p))
  `(progn (declaim (special ,n))
          ,(if i-p `(unless (boundp ',n) (set ',n ,i)))
          ,(if d-p `(%SET-DOCUMENTATION ',n 'variable ',d))
          ',n))

(defmacro defparameter (n i &optional (d nil d-p))
  `(progn (declaim (special ,n))
          (set ',n ,i)
          ,(if d-p `(%SET-DOCUMENTATION ',n 'variable ',d))
          ',n))

(defun list-length-proper (x)
  (multiple-value-bind (len end) (%proper-list x)
    (if (or end (null len))
      (err))
    len))
    
(defun proper-list-p (x)
  (multiple-value-bind (len end) (%proper-list x)
    (and len (null end))))

(defun proper-list-length-in-bounds-p (x n &optional m)
  (multiple-value-bind (len end) (%proper-list x)
    (and len (null end) (>= len n) (or (null m) (>= m len)))))

(defun list-length-in-bounds-p (x n m restp)
 	(dotimes (i n)
		(if (atom x) (return-from list-length-in-bounds-p nil))
 		(setq x (cdr x)))
 	(if (< m n) (return-from list-length-in-bounds-p nil)) 	
 	(dotimes (i (- m n))
		(cond ((null x) (return-from list-length-in-bounds-p t))
 			  ((atom x) (return-from list-length-in-bounds-p nil)))
 		(setq x (cdr x)))
 	(or restp (null x)))	

(defvar *DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST* nil)




  
#|
(defun string (s)
  (cond ((stringp s) s)
        ((symbolp s) (symbol-name s))
        ((characterp s) (make-string 1 :initial-element s))
        ((error 'type-error))))
|#


(defun _check-contents (dims seq)
  (if dims
    (let ((n (car dims))
          (r (cdr dims)))
      (if (/= n (length seq))
        (error t)
        (do ((i 0 (1+ i)))
            ((= i n))
          (_check-contents r (elt seq i)))))))

(defun _check-non-negative (x)
  (unless (and (integerp x) (not (minusp x)))
    (error 'type-error :datum x :expected-type '(integer 0)))
  x)
                  
(defsetf row-major-aref _setf_row-major-aref)

#|
;!!!
(_pr "----------row-major-aref---------------")
(multiple-value-bind (a b c d e)
    (get-setf-expansion '(row-major-aref a i))
  (_pr a)  
  (_pr b)  
  (_pr c)  
  (_pr d)  
  (_pr e))
|#          

(defun _set-array-element (a j dims seq)
  (if dims
    (let ((n (car dims))
          (r (cdr dims)))
      (do ((i 0 (1+ i)))
          ((= i n) j)
        (setq j (_set-array-element a j r (elt seq i)))))
    (progn (setf (row-major-aref a j) seq)
           (1+ j))))

(defun _fill-array-contents (a seq)
  (_check-contents (array-dimensions a) seq)
  (_set-array-element a 0 (array-dimensions a) seq))                   

(defun upgraded-array-element-type (x &optional env)
		(declare (ignore env))
  (case x
    (bit 'bit)
    ((standard-char base-char) 'base-char)
    ((extended-char character) 'character)
    (t t)))  

#|!!!R
(defun make-array (dims &key (element-type t) initial-element (initial-contents nil cont-suppl)
                   adjustable fill-pointer displaced-to (displaced-index-offset 0))
  (let ((a (_make-array dims (upgraded-array-element-type element-type) initial-element adjustable
               fill-pointer displaced-to displaced-index-offset)))
    (if cont-suppl (_fill-array-contents a initial-contents))
    a))
|#    

(defsetf aref store)  


  
#|!!! as SUBR
(defun svref (v i)
  (if (simple-vector-p v)
    (aref v i)
    (error 'type-error)))
|#

(defun (setf svref) (n v i)
  (if (simple-vector-p v)
    (setf (aref v i) n)
    (error 'type-error)))  


(defun complexp (x)
  (eq (type-of x) 'complex))

(defun _check-chars (r)
  (if r (if (characterp (car r)) (_check-chars (cdr r))
                                 (error 'type-error))))

(defun _char-compare-1 (f g a r)
  (if r (let ((b (car r)))
          (and (funcall f (funcall g a) (funcall g b)) (_char-compare-1 f g b (cdr r))))
        t))

(defun _char-compare (f a r)
  (_check-chars (cons a r))
  (_char-compare-1 f #'char-code a r))

(defun _char-compare-ex (f a r)
  (_check-chars (cons a r))
  (_char-compare-1 f #'(lambda (ch)
                         (char-code (char-upcase ch)))
                     a r))

(defun char= (a &rest r)
  (_char-compare #'= a r))

(defun char/= (a &rest r)
  (_check-chars (cons a r))
  (apply #'_/= #'char= a r))

(defun char< (a &rest r)
  (_char-compare #'< a r))

(defun char> (a &rest r)
  (_char-compare #'> a r))

(defun char<= (a &rest r)
  (_char-compare #'<= a r))

(defun char>= (a &rest r)
  (_char-compare #'>= a r))

(defun char-equal (a &rest r)
  (_char-compare-ex #'= a r))

(defun char-not-equal (a &rest r)
  (_check-chars (cons a r))
  (apply #'_/= #'char-equal a r))

(defun char-lessp (a &rest r)
  (_char-compare-ex #'< a r))

(defun char-greaterp (a &rest r)
  (_char-compare-ex #'> a r))

(defun char-not-lessp (a &rest r)
  (_char-compare-ex #'>= a r))

(defun char-not-greaterp (a &rest r)
  (_char-compare-ex #'<= a r))

(defun _minmax (f a r)
  (if r
    (_minmax f (if (funcall f a (car r)) a (car r)) (cdr r))
    a))

(defun max (a &rest r)
  (if (fboundp 'EXT::type-expand)	;!!! dont work befor "type.lisp" loaded
  	(check-type a real))
  (_minmax '> a r))

(defun min (a &rest r)
  (if (fboundp 'EXT::type-expand)
  	(check-type a real))
  (_minmax '< a r))

#|!!!R     hardwired
(defun _mul2 (x y)
  (cond ((and (integerp x) (integerp y)) (_mul x y))
        ((floatp x) (_mul x y))
        ((rationalp x) (/ (* (numerator x) (numerator y))
                          (* (denominator x) (denominator y))))
        ((complexp x) (let ((a (realpart x))
                            (b (imagpart x))
                            (c (realpart y))
                            (d (imagpart y)))
                        (complex (- (* a c) (* b d)) (+ (* c b) (* a d)))))))


(defun abs (z)
  (typecase z
    (complex (let ((x (realpart z))
                    (y (imagpart z)))
                   (sqrt (+ (* x x) (* y y)))))
    (real (if (minusp z) (- z) z))
	(t (error 'type-error :datum z :expected-type 'number))))





(defun _euclid (f x y)
  (multiple-value-bind (n a)
			(funcall f x y)
	(declare (ignore n))
    (if (zerop a) y (_euclid f y a))))

(defun gcd (&rest r)
  (if r (let ((a (abs (car r)))
              (d (cdr r)))
          (if d (apply #'gcd (_euclid #'floor a (abs (car d))) (cdr d))
                a))
        0))
|#


#|!!!        
(defun _div-int (x y)
  (when (minusp y)
    (setq y (- y))
    (setq x (- x)))
  (let ((g (gcd x y)))
    (when (/= g 1)
      (setq x (floor x g))
      (setq y (floor y g)))
    (if (= y 1) x
                (_make-ratio x y))))
|#                

#|Hardcoded as _make-ratio
(defun _div-int (x y)
  (when (minusp y)
    (setq y (- y))
    (setq x (- x)))
  (let ((g (gcd x y)))
    (_make-ratio (truncate x g) (truncate y g))))
|#


#|!!! hardwired


(defun _div2 (x y)
  (if (zerop y) (error 'division-by-zero))
  (cond ((integerp x) (_make-ratio x y))
        ((floatp x) (_div x y))
        ((rationalp x) (/ (* (numerator x) (denominator y))
                          (* (numerator y) (denominator x))))
        ((complexp x) (let* ((a (realpart x))
                             (b (imagpart x))
                             (c (realpart y))
                             (d (imagpart y))
                             (m (+ (* c c) (* d d))))
                        (complex (/ (+ (* a c) (* b d)) m)
                                 (/ (- (* c b) (* a d)) m))))))

(defun * (&rest r)
  (_check-numbers r)
  (_reduce-list #'_mul2 r 1))

(defun / (a &rest r)
  (_check-numbers (cons a r))
  (_reduce-list-ex #'_div2 a r 1))
|#


(defun assoc-if (p list &key key)
  (car (_list-test #'car p list (_key key))))

(defun assoc-if-not (p list &key key)
  (assoc-if (complement p) list :key key))

(defun rassoc-if (p list &key key)
  (car (_list-test #'cdr p list (_key key))))

(defun rassoc-if-not (p list &key key)
  (rassoc-if (complement p) list :key key))

(defun rassoc (item list &key key (test #'eql) test-not)
  (rassoc-if (_test item test test-not) list :key key))

(defun set-difference (list-1 list-2 &rest rest &key key (test #'eql) test-not)
		(declare (ignore test test-not))
  (if list-1 (let ((r (apply #'set-difference (cdr list-1) list-2 rest))
                   (item (car list-1)))
               (if (apply #'member (funcall (_key key) item) list-2 rest) (cons item r)
                                                                                          r))))

(defun nset-difference (list-1 list-2 &rest rest &key key (test #'eql) test-not)
		(declare (ignore key test test-not))
  (apply #'set-difference list-1 list-2 rest))

(defun adjoin (item list &rest rest &key key (test #'eql) test-not)
		(declare (ignore test test-not))
  (if (apply #'member (funcall (_key key) item) list rest) list
                                                   (cons item list)))

(defun reverse (seq)
  (nreverse (copy-seq seq)))



#|!!!D
  (do ((nseq (copy-seq seq))
       (i 0 (1+ i))
       (n (length seq)))
      ((= i n) nseq)
    (setf (elt nseq i) (elt seq (- n i 1)))))
|#    

(defun _sharp-quote (stm ch p)
		(declare (ignore ch))
  (list 'function (read stm t nil t)))

(defun _rev-list-to-string (list)
  (concatenate 'string (nreverse list)))

(defun _sharp-backslash (stm ch p)
		(declare (ignore ch p))
  (let ((a (read-char stm)))
    (if (eq (_char-type (peek-char nil stm)) :constituent)
      (let ((r (name-char (_rev-list-to-string (do ((p (list a))
                                                    (ch (peek-char nil stm) (peek-char nil stm)))
                                                   ((not (and ch (eq (_char-type ch) :constituent))) p)
                                                 (push ch p)
                                                 (read-char stm))))))
        (if r r
              (error 'reader-error)))
      a)))

(defun _sharp-colon (stm ch p)
		(declare (ignore ch p))
  (_read-uninterned stm))

(defun _sharp-comma (stm ch p)
		(declare (ignore ch p))
  (let ((x (read stm t nil t)))
    (unless *read-suppress* (eval x))))

#|!!!C    
(defun _sharp-dot (stm ch p)
  (let ((token (read stm t nil t)))
    (unless *read-suppress*
      (unless *read-eval* (error 'reader-error))
      (eval token))))
|#
 

#|  
(defun _sharp-cond (stm f)
  (if (funcall f (let ((*package* (find-package 'keyword)))
                   (read stm t nil t))
                 *features*)
    (read stm t nil t)
    (let ((*read-suppress* t))
       (read stm)
       (values))))

(defun _sharp-plus (stm ch p)
  (_sharp-cond stm #'member))

(defun _sharp-minus (stm ch p)
  (_sharp-cond stm (complement #'member)))
  |#

(defun _sharp-equal (stream ch n)
		(declare (ignore ch))
	(if *read-suppress* (values)
      (if n (let ((h (assoc n *read-reference-table*)))
             (if (consp h)
               (error "~ of ~: Label #~= must not be defined twice." 'read stream n)
               (let ((label (make-read-label n)))
                 (push (setq h (cons n label)) *read-reference-table*)
                 (let ((obj (read stream t nil t)))
                   (if (eq obj label)
                     (error "~ of ~: #~= #~# is not allowed." 'read stream n n)
                     (setf (cdr h) obj))))))
            (error "~ of ~: Between # ecified." 'read stream))))         
         
(defun _sharp-sharp (stream ch n)
		(declare (ignore ch))
	(unless *read-suppress*
    (if n (let ((h (assoc n *read-reference-table*)))
            (if (consp h)
               (cdr h) ; will be disentangled later  you could also return (cdr h) 
               (error "~ of ~: Label #~= is not defined." 'read stream n)))
          (error "~ of ~: Between # and #ied." 'read stream))))
  
  
  
(let ((nrt *_standard-readtable*))
;  (set-macro-character #\) #'_rpar-reader nil nrt)
;  (set-macro-character #\' #'_quote-reader nil nrt)
;  (set-macro-character #\; #'_semicolon-reader nil nrt)
;  (set-macro-character #\" #'_dquote-reader nil nrt)
;  (make-dispatch-macro-character #\# nil nrt)
;  (set-dispatch-macro-character #\# #\' #'_sharp-quote nrt)
;  (set-dispatch-macro-character #\# #\\ #'_sharp-backslash nrt)
;  (set-dispatch-macro-character #\# #\: #'_sharp-colon nrt)
;!!!C  (set-dispatch-macro-character #\# #\. #'_sharp-dot nrt)
;  (set-dispatch-macro-character #\# #\S #'_sharp-s nrt)
;  (set-dispatch-macro-character #\# #\+ #'_sharp-plus nrt)
;  (set-dispatch-macro-character #\# #\- #'_sharp-minus nrt)

  (set-dispatch-macro-character #\# #\, #'_sharp-comma nrt)
  (set-dispatch-macro-character #\# #\, #'_sharp-comma *readtable*)

  (set-dispatch-macro-character #\# #\= #'_sharp-equal nrt)
  (set-dispatch-macro-character #\# #\= #'_sharp-equal *readtable*)

  (set-dispatch-macro-character #\# #\# #'_sharp-sharp nrt)
  (set-dispatch-macro-character #\# #\# #'_sharp-sharp *readtable*)
  
  
  )

(defconstant LAMBDA-PARAMETERS-LIMIT #+CLISP 4096
                                     #-CLISP 8192)
                                     
(defconstant CALL-ARGUMENTS-LIMIT LAMBDA-PARAMETERS-LIMIT)

(defconstant %TYPE-INPUT-STREAM '(SATISFIES INPUT-STREAM-P))  
(defconstant %TYPE-OUTPUT-STREAM '(SATISFIES OUTPUT-STREAM-P))  


(defun TEXT (s)
	s)		
	
(defun char (v i)
  (if (stringp v)
    (aref v i)
    (error 'type-error :datum v :expected-type 'string)))

(defun (setf char) (n v i)
  (if (stringp v)
    (setf (aref v i) n)
	(error 'type-error :datum v :expected-type 'string)))

#|!!!    
(setq *error-handler* #'(lambda (&rest r)
  (let ((*error-handler* nil))
    (_pr "###################### ERROR #####################")
		(dolist (x r)
			(_pr x))
		(err))))
|#
		
(defun warn (&rest r)
	(_pr ".............WARN..............")
	(dolist (x r)
		(_pr x))
	nil)

(defun _err (cnd code &rest args)
;  (apply #'format nil (_get-err-message code) args)
  (error cnd (apply #'format nil (_get-err-message code) args)))
  
	
(defun error (errorstring &rest args)
;;  (_pr "----error----")  
;;  (_pr errorstring)
;  (_pr args)

  (if (or *error-handler* (not *use-clcs*))
    (progn
      (if *error-handler*
        (apply *error-handler* nil errorstring args)
        (progn
          (terpri *error-output*)
          (write-string "*** - " *error-output*)
          (apply #'format *error-output* errorstring args)))
      (funcall *break-driver* nil))
    (let ((condition (coerce-to-condition errorstring args 'error 'simple-error)))
      (signal condition)
      (invoke-debugger condition))))
		
(defun error-of-type (type &rest arguments)
 ;; split off keyword arguments from the &rest arguments:
 (let ((keyword-arguments '()))
   (loop
     (unless (and (consp arguments) (symbolp (car arguments))) (return))
     (push (pop arguments) keyword-arguments)
     (push (pop arguments) keyword-arguments))
   (setq keyword-arguments (nreverse keyword-arguments))
   (let ((errorstring (first arguments))
         (args (rest arguments)))
     (if (or *error-handler* (not *use-clcs*))
       (progn
         (if *error-handler*
           (apply *error-handler* nil errorstring args)
           (progn
             (terpri *error-output*)
             (write-string "*** - " *error-output*)
             (apply #'format *error-output* errorstring args)))
         (funcall *break-driver* nil))
       (let ((condition
               (apply #'coerce-to-condition errorstring args
                      'error (convert-simple-condition type)
                      keyword-arguments)))
         (signal condition)
         (invoke-debugger condition))))))    
 
(defun acons (x y a)
  (cons (cons x y) a))  

(defun _check-bi (seq start end)
  (unless (and (integerp start)
               (integerp end)
               (<= 0 start end (length seq)))
    (err)))
  
(defun mismatch (seq1 seq2 &key from-end (test #'eql) test-not key (start1 0) (start2 0) end1 end2)
  (setq key (_key key)
        end1 (_end end1 seq1)
        end2 (_end end2 seq2))
  (_check-bi seq1 start1 end1)
  (_check-bi seq2 start2 end2)
  (do ((i (if from-end (1- end1) start1) (if from-end (1- i) (1+ i)))
       (j (if from-end (1- end2) start2) (if from-end (1- j) (1+ j)))
       (pred (if test-not (complement test-not) test)))
      (nil)
    (let ((ppi (if from-end (< i start1) (>= i end1)))
          (pj (if from-end (< j start2) (>= j end2))))
      (cond ((and ppi pj) (return-from mismatch nil))
            ((or ppi pj) (return-from mismatch i))))
    (let ((a (funcall key (elt seq1 i)))
          (b (funcall key (elt seq2 j))))
      (unless (funcall pred a b)
        (return-from mismatch (if from-end (1+ i) i))))))
  

(defun string= (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (not (apply #'mismatch (string x) (string y) :test #'char= rest)))

(defun string-equal (x y &rest rest &key (start1 0) end1 (start2 0) end2)
		(declare (ignore start1 end1 start2 end2))
  (not (apply #'mismatch (string x) (string y) :test #'char-equal rest)))  
  
(defparameter
  *_char-name-alist*
  (labels ((_make-names (r)
             (if r (acons (car r)
                          (code-char (cadr r))
                          (_make-names (cddr r))))))
    (_make-names '("NULL"      0
                   "BELL"      7
                   "BACKSPACE" 8
                   "BS"        8
                   "TAB"       9
                   "NEWLINE"   10
                   "NL"        10
                   "LINEFEED"  10
                   "LF"        10
                   "VT"        11
                   "PAGE"      12
                   "FORM"      12
                   "FORMFEED"  12
                   "FF"        12
                   "RETURN"    13
                   "CR"        13
                   "ESCAPE"    27
                   "ESC"       27
                   "ALTMODE"   27
                   "ALT"       27
                   "SPACE"     32
                   "SP"        32
                   "DELETE"    127
                   "RUBOUT"    127))))

(defun name-char (name)
  (cdr (assoc (string name) *_char-name-alist* :test #'string-equal)))  
  
(defun make-string (size &key (initial-element (code-char 32)) (element-type 'character))
	(if (or (eq element-type 'character) (subtypep element-type 'character))  ;!!! subtype don't work during boot
  		(make-array size :initial-element initial-element :element-type element-type :adjustable t :fill-pointer t)
  		(err)))

(defun bit (a &rest r)
  (apply #'aref a r))  
  
(defun sbit (a &rest r)
  (apply #'aref a r))
  
(defsetf bit store)  

(defsetf sbit store)  
  
(defvar *_trace* nil)

(defun _trace (list)
  (if list
    (dolist (x list (_update-trace))
      (if (and (fboundp x) (functionp (fdefinition x)))
        (setq *_trace* (adjoin x *_trace* :test #'equal))
        (error 'simple-error "Function ~s not defined" x)))
    *_trace*))

(defmacro trace (&rest x)
  `(_trace ',x))

(defun _untrace (list)
  (setq *_trace* (if list (set-difference *_trace* list) nil))
  (_update-trace))

(defmacro untrace (&rest x)
  `(_untrace ',x))

(defun _trace-enter (name level args)
  (format *trace-output* "Trace:~A ~A ~S~%" (make-string level :initial-element #\Space) name args))
      
(defun _trace-exit (name level vals)
  (format *trace-output* "Trace:~A ~A returns" (make-string level :initial-element #\Space) name)
  (dolist (x vals)
    (format *trace-output* " ~S" x))
  (fresh-line *trace-output*))



(%putd 'set-symbol-value (symbol-function 'set))

(sys::%putd 'check-symbol
  (function check-symbol (lambda (object caller)
														(unless (symbolp object)
															(error-of-type 'source-program-error
																(TEXT "~S: ~S is not a symbol.")
																caller object))
														object)))    
        
        

(sys::%putd '%the-environment
  (function %the-environment (lambda (form env)
    (declare (ignore form))
    (sys::svstore env 0 (svref (svref env 0) 2)) ; nuke *evalhook* binding
    env)))
(sys::%putd '%uncompilable
  (function %uncompilable (lambda (form)
    (error-of-type 'source-program-error
      (TEXT "~S is impossible in compiled code")
      form))))
(sys::%putd 'the-environment
  (sys::make-macro
    (function the-environment (lambda (form env)
      (declare (ignore form env))
      '(progn
        (eval-when ((not eval)) (%uncompilable 'the-environment))
        (let ((*evalhook* #'%the-environment)) 0))))))
        
        
(defun svstore (a i x) ;!!!
  (setf (svref a i) x))
  
(defun %svstore (x a i) ;!!!
  (setf (svref a i) x))
  
(defun get-funname-symbol (funname)
  (if (atom funname) funname
    (get-setf-symbol (second funname))))
    
#-compiler
(defmacro COMPILER::EVAL-WHEN-COMPILE (&body body) ; preliminary
  `(eval-when (compile) ,@body))
  
  
(PROGN


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

(predefun check-not-declaration (symbol caller) (check-symbol symbol caller))

(VALUES) )


    
(_load "seq")                
(_load "defseq")

(defconstant most-negative-fixnum (1- (- most-positive-fixnum)))

(defconstant array-rank-limit most-positive-fixnum)
(defconstant array-dimension-limit most-positive-fixnum)
(defconstant array-total-size-limit most-positive-fixnum)

(defun array-dimension (a n)
  (nth n (array-dimensions a)))

(defun array-total-size (a)
  (apply #'* (array-dimensions a)))

(defun array-in-bounds-p (a &rest r)
  (and (not (some #'minusp r))
       (every #'< r (array-dimensions a))))

;!!! (defsetf documentation %set-documentation)



#|!!!
(defun _copy-array (pa na y &optional x)
  (if y (dotimes (i (car y))
          (_copy-array pa na (cdr y) (append x (list i))))
        (setf (row-major-aref na (apply #'array-row-major-index na x)) (apply #'aref pa x))))

|#

#|
!!!
  (if y (do ((i (1- (car y)) (1- i)))
            ((minusp i))
          (_copy-array pa na (append x (list i)) (cdr y)))
        (setf (aref na ,@x) (aref pa ,@x))))
|#

#|!!!R
(defun adjust-array (a dims &rest rest &key (element-type nil et-sp) initial-element (initial-contents nil cont-suppl) fill-pointer
                     displaced-to (displaced-index-offset 0))
  (setq element-type (if et-sp (upgraded-array-element-type element-type) (array-element-type a)))
  ;; all our arrays adjustable
  (_adjust-array a dims element-type initial-element fill-pointer
                                    displaced-to displaced-index-offset))
;!!!    (if cont-suppl (_fill-array-contents na initial-contents)
;!!!                   (_copy-array pa na (mapcar #'min (array-dimensions pa) (array-dimensions na))))

|#

(defun char-name (ch)
  (car (rassoc ch *_char-name-alist*)))

(defun replace (seq-1 seq-2 &key (start1 0) end1 (start2 0) end2)
  (setq end1 (_end end1 seq-1)
				end2 (_end end2 seq-2))
  (let ((n (min (- end1 start1) (- end2 start2)))
        (seq (if (eq seq-1 seq-2) (copy-seq seq-2) seq-2)))
    (dotimes (i n seq-1)
      (setf (elt seq-1 (+ start1 i)) (elt seq (+ start2 i))))))

(defun (setf subseq) (new seq start &optional end)
  (replace seq new :start1 start :end1 end)
  new)

(defun _pk (p key)
  (if key #'(lambda (a b) (funcall p (funcall key a) (funcall key b)))
          p))

(defun tailp (x y)
  (or (eql x y) (if (not (atom y)) (tailp x (cdr y)))))

(defun ldiff (x y)
  (if (not (eql x y)) (if (atom x)
                        x
                        (cons (car x) (ldiff (cdr x) y)))))

(defun butlast (a &optional (n 1))
;  (check-type n (integer 0))
  (ldiff a (last a n)))

(defun nbutlast (a &optional (n 1))
;  (check-type n (integer 0))
  (when (> (length a) n)
		(rplacd (last a (1+ n)) nil)
		a))
          
(defun revappend (a b)
  (nconc (reverse a) b))

(defun copy-tree (a)
  (if (consp a) (cons (copy-tree (car a))
                      (copy-tree (cdr a)))
                a))
                
(defun subst-if (new p tree &key key)
  (cond ((funcall p (funcall (_key key) tree)) new)
        ((atom tree) tree)
        ((cons (subst-if new p (car tree) :key key)
               (subst-if new p (cdr tree) :key key)))))	

(defun subst-if-not (new p tree &key key)
	(subst-if new (complement p) tree :key key))

(defun subst (new old tree &key key test test-not)
  (subst-if new (_test old test test-not) tree :key key))
  
(defun nsubst-if (new p tree &key key)
  (cond ((funcall p (funcall (_key key) tree)) new)
        (t (when (consp tree)
  			     (rplaca tree (nsubst-if new p (car tree) :key key))
  					 (rplacd tree (nsubst-if new p (cdr tree) :key key)))
  				 tree)))
  
(defun nsubst-if-not (new p tree &key key)
	(nsubst-if new (complement p) tree :key key))
  
(defun nsubst (new old tree &key key test test-not)
  (nsubst-if new (_test old test test-not) tree :key key))
        
(defun sublis (alist tr &rest rest &key key (test #'eql) test-not)
	(let ((a (assoc (funcall (_key key) tr) alist :test test :test-not test-not)))
		(if a (cdr a)
	  			(if (atom tr) tr
	  										(cons (apply #'sublis alist (car tr) rest)
	  													(apply #'sublis alist (cdr tr) rest))))))

(defmacro typecase (expr &rest clauses)
  (let* ((temp (gensym))
	 (type-list nil)
	 (body (cons 'cond
		     (mapcar
                       #'(lambda (c)
                           (let ((a (car c)))
                             (cons (cond ((eq a 'otherwise) t)
				         ((eq a 'ecase-error-flag)
                                            `(error "etypecase failed: %s, %s" ,temp ',(reverse type-list)))
                                         (t (push a type-list)
                                            (list 'typep temp (if (and (listp a) (not (memq (car a) '(and not member))))
                                                                `',(cons 'or a)
                                                                `',a))))
                                   (or (cdr c) '(nil)))))
		       clauses))))
    `(let ((,temp ,expr)) ,body)))

(defun pairlis (keys data &optional alist)
  (dolist (k keys)
    (if (atom data) (error 'type-error :datum data :expected-type 'cons))
    (push (cons k (car data)) alist)
    (setq data (cdr data)))
  (if data (err))
  alist)

(defun copy-alist (alist)
  (mapcar #'(lambda (x) (if (consp x) (cons (car x) (cdr x)) x))
          alist))
#|!!!
  (if alist (cons (if (atom (car alist)) (car alist) (cons (caar alist) (cdar alist)))
                  (copy-alist (cdr alist)))))
|#                  
  
                                        
(defun tree-equal (x y &rest rest &key (test #'eql) test-not)
  (cond ((consp x) (and (consp y)
											  (apply #'tree-equal (car x) (car y) rest)
											  (apply #'tree-equal (cdr x) (cdr y) rest)))
				((and (atom y) (funcall (_test x test test-not) y)))))

(defmacro multiple-value-list (f)
  `(multiple-value-call #'list ,f))
  
(defmacro multiple-value-setq (varlist form)
  (let ((g (gensym))
        (poplist nil))
    (dolist (var varlist) (setq poplist (cons `(SETQ ,var (POP ,g)) poplist)))
    `(LET* ((,g (MULTIPLE-VALUE-LIST ,form)))
       ,(if poplist `(PROG1 ,@(nreverse poplist)) NIL))))

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

(defmacro check-type (place typespec &optional (string nil) &environment env)
  (let ((tag1 (gensym "CHECK-TYPE-"))
        (tag2 (gensym "OK-"))
        (var (gensym)))
    `(TAGBODY
       ,tag1
       (LET ((,var ,place))
         (WHEN (TYPEP ,var ',typespec) (GO ,tag2))
         (CHECK-TYPE-FAILED ',place ,var
                            #'(LAMBDA (NEW-VALUE) (SETF ,place NEW-VALUE))
                            ,(length (nth-value 2 (get-setf-expansion place env)))
                            ,string ',typespec))
       (GO ,tag1)
       ,tag2)))

#|!!!R
(defmacro check-type (p ts &optional s)
;		(declare (ignore p ts s))
  )
|#


#|
  (cond ((null vars) `(progn ,form nil))
	((null (cdr vars)) `(setq ,(car vars) (car ,form)))
	(t
	 (let* ((temp (gensym)) (n 0))
	   (list 'let (list (list temp (list 'multiple-value-list form)))
		 (list 'prog1 (list 'setq (pop vars) (list 'car temp))
		       (cons 'setq (apply 'nconc
					  (mapcar (function
						   (lambda (v)
						     (list v (list
							      'nth
							      (setq n (1+ n))
							      temp))))
						  vars)))))))))
|#

(defun copy-symbol (sym &optional copy-props)
  (let ((ns (make-symbol (symbol-name sym))))
    (when copy-props 
      (if (boundp sym) (setf (symbol-value ns) (symbol-value sym)))
      (if (fboundp sym) (setf (symbol-function ns) (symbol-function sym)))
      (setf (symbol-plist ns) (copy-list (symbol-plist sym))))
    ns))

(defvar *load-pathname* nil)
(defvar *load-truename* nil)
(defparameter *load-print* nil)
(defvar *load-verbose* nil)


(defun (setf fdefinition) (f s)
  (setf (symbol-function (if (_setf-name-p s) (get-setf-symbol (cadr s))
                                              s))
        f))

(defun (setf char) (c s i)
  (setf (aref s i) c))

;!!!D(defsetf slot-value _setf_slot-value)

#|!!!
(defun _tree-member (x a)
  (if a (or (member x (car a)) (_tree-member x (cdr a)))))
|#

(defun listify (r)
  (if (listp r) r
                (list r)))

(defun _read_keys (a)
  (if a (list* (_to-keyword (car a)) (cadr a) (_read_keys (cddr a)))))

(defvar *args* nil) 
(defvar *error-handler* nil)
(defvar *use-clcs* nil)
(defvar *break-driver* nil)
(defvar *break-on-signals* nil)

(defun coerce-to-condition (datum &rest rest)
		(declare (ignore rest))
	datum)

(defun signal (datum &rest args)
  (let ((cond (coerce-to-condition datum args 'signal 'simple-condition)))
  	(if (and *break-on-signals* (safe-typep cond *break-on-signals*))
  	  (funcall *break-driver* t cond t))
  	(_invoke-handlers cond)
  	nil))



;(defvar *features* '(:ufasoft-lisp :ansi-cl :clos :loop :common-lisp)) ;::UNICODE


(defun close (stream &key abort)
  (built-in-stream-close stream :abort abort))

(defsetf readtable-case _setf_readtable-case)

#|!!!

(defun _get-input-stream (x)
  (cond ((streamp x) x)
        ((null x) *standard-input*)
        ((eq x t) *terminal-io*)
        ((error 'stream-error))))


(defun read-char (&optional stm (eof-error-p t) eof-value recursive-p)
  (or (_read-char (_get-input-stream stm))
      (if eof-error-p (error 'end-of-file)
                      eof-value)))

(defun peek-char (&optional peek-type stm (eof-error-p t) eof-value recursive-p &aux ch)
  (do ()
      (nil)
    (setq ch (read-char stm eof-error-p nil recursive-p))
    (cond ((null ch) (if recursive-p (error 'end-of-file)
                                     (return-from peek-char eof-value)))
          ((null peek-type) (return))
          ((eq peek-type t) (unless (eq (_char-type ch) :whitespace)
                                    (return)))
          ((char= ch peek-type) (return))))
  (unread-char ch stm)
  ch)
|#

#|
(defun get-macro-character (ch &optional (rt *readtable*))
  (multiple-value-bind (a p) (_char-type ch)
    (if (functionp a) (values a (cdr p)) (values nil nil))))

(defun set-macro-character (ch f &optional ntp (rt *readtable*))
  (_setf_char-type ch rt f nil) ;!!! need non-terminate attribute
  t)

(defun get-dispatch-macro-character (disp-char sub-char &optional (rt *readtable*))
  (setq sub-char (char-upcase sub-char))
  (multiple-value-bind (a p) (_char-type disp-char rt)
    (if (functionp a) (cdr (assoc sub-char p))
                      (error 'reader-error))))


(defun set-dispatch-macro-character (disp-char sub-char f &optional (rt *readtable*))
  (if (digit-char-p sub-char) (error 'reader-error))
  (setq sub-char (char-upcase sub-char))
  (multiple-value-bind (ct p) (_char-type disp-char rt)
		(if (eq ct (get-macro-character #\# nil)) (setf (getf p sub-char) f)
																							(error 'reader-error))
  t)



(defun _macro-dispatcher (stm ch &aux p)
  (do ((p nil (if p (+ (* p 10) (digit-char-p c))
                    (digit-char-p c)))
       (c (read-char stm) (read-char stm)))
      ((not (digit-char-p c)) (let ((f (get-dispatch-macro-character ch c)))
                                    (if f (funcall f stm c p)
                                          (error 'reader-error))))))
|#

(defun make-dispatch-macro-character (ch &optional ntp (rt *readtable*))
  (set-macro-character ch (get-macro-character #\# nil) ntp rt))

(defun read-line (&optional stm (eof-error-p t) eof-value recursive-p) ;!!!
		(declare (ignore recursive-p))
  (do ((p nil (cons ch p))
       (ch (read-char stm nil) (read-char stm nil)))
      ((eql ch #\Newline) (values (ext:string-concat (nreverse p)) nil))
    (unless ch      
      (return (values (if p (ext:string-concat (nreverse p))
                          (if eof-error-p (err) eof-value))
                      t)))))


#|!!!
(defun read-delimited-list (char &optional stm recursive-p)
  (do (p
       q)
      (nil)
    (let ((ch (peek-char nil stm)))
      (cond ((null ch) (error 'stream-error))
            ((eql ch char) (read-char stm)
                           (return p))
            ((eq (_char-type ch) :constituent) (let ((r (cons (read stm) nil)))
                                                 (if q (rplacd q r)
                                                       (setq p r))
                                                 (setq q r)))
            ((read-char stm))))))
|#

(defun _read-mescape (stm)
  (let* ((ch (read-char stm))
         (typ (if ch (_char-type ch) (error 'reader-error))))
    (cond ((eq typ :mescape) nil)
          ((eq typ :sescape) (cons (read-char stm) (_read-mescape stm)))
          ((cons ch (_read-mescape stm))))))

#|!!!R
(defun _as-integer (x &optional (radix 10) minus)
  (let ((c (car x))
        (r (cdr x))
        d)
    (cond ((eql c #\-) (_as-integer r radix t))
          ((eql c #\+) (_as-integer r radix nil))
          ((null x) 0)
          ((setq d (digit-char-p c)) (if r (+ (* d radix) (_as-integer r radix minus))
                                           (if minus (- d) d))))))

(defun _decimal-integer (x)
  (if (digit-char-p (car x)) (let ((r (cdr x)))
                               (if (and (_decimal-point (car r)) (null (cdr r)))
                                 t
                                 (_decimal-integer r)))))

|#


;(defvar *read-suppress* nil)
;!!!C (defvar *read-eval*     t)
;!!!(defvar *read-base*     10)
(defvar *read-default-float-format* 'single-float)

(defun _update-case (x c &aux p q)
  (dolist (a x p)
    (let ((r (cons (cond ((eq c :upcase) (char-upcase a))
                         ((eq c :downcase) (char-downcase a))
                         ((eq c :preserve) a)
                         ((eq c :invert) (if (upper-case-p a) (char-downcase a) (char-upcase a)))
                         ((error 'reader-error)))
                   nil)))
      (if q (rplacd q r)
            (setq p r))
      (setq q r))))


#|
                       (:upcase #'char-upcase)
                       (:downcase #'char-downcase)
                       (:preserve #'identity)
                       (:invert (lambda (a) (if (upper-case-p a) (char-downcase a) (char-upcase a))))
                       (t (error 'reader-error)))))
|#

#|!!!
  (let* ((f (case (readtable-case *readtable*)
              (:upcase #'char-upcase)
              (:downcase #'char-downcase)
              (:preserve #'identity)
              (:invert (lambda (a) (if (upper-case-p a) (char-downcase a) (char-upcase a))))
              (t (error 'reader-error))))
         (n (do ((y x (cdr y))
                 (i 0 (1+ i)))
                ((eq y end) i)
              ))
         (s (make-string n)))
    (dotimes (i n s)
      (setf (aref s i) (funcall f (caar x)))
      (setq x (cdr x)))))
|#

(defvar *_trace_* nil) ;!!!


#|!!!

(defun _read-token-list (stm &optional pres-white)
  (let ((ch (peek-char nil stm nil)))
    (if ch (let ((typ (_char-type ch)))
             (cond ((eq typ :constituent) (_read-simple-token stm pres-white))
                   ((eq typ :mescape) (read-char stm)
                                      (_read-mescape stm))
                   ((eq typ :sescape) (read-char stm)
                                      (cons (cons (read-char stm) _alphabetic) (_read-simple-token stm pres-white)))
                   (nil))))))

(defun _chain-s (x &optional end)
  (_chain-s-ex x end (readtable-case *readtable*)))

(defun _read-uninterned (stm)
  (make-symbol (_chain-s (_read-token-list stm))))


(defconstant _dot_ex '(dot))
(defconstant _rpar '(rpar))

(defun _dots (x)
  (if x (if (logtest _dot (cdar x)) (_dots (cdr x)))
        t))

(defun _integer-token-p (x &optional (radix *read-base*))
  (let* ((s (cdar x))
         (y (if (logtest _sign s) (cdr x)
                                x))
         (r 0))
    (if (logtest _point (cdar (last y)))
      (dolist (el y)
        (let ((d (digit-char-p (car el))))
          (if (logtest _point (cdar (last y))) (return))
          (if (and d (logtest _alphadigit (cdr el)))
            (setq r (+ (* r 10) d))            
            (return-from _integer-token-p nil))))
      (dolist (el y)
        (let ((d (digit-char-p (car el) radix)))
          (if (and d (logtest _alphadigit (cdr el)))
            (setq r (+ (* r radix) d))
            (return-from _integer-token-p nil)))))
    (if (logtest _minus s) (- r) r)))

(defun _ratio-token-p (x &optional (radix 10))
  (let ((p (_member-trait _ratio x)))
    (if p (let ((n (ldiff x p))
                (d (cdr p)))
            (/ (_integer-token-p n) (_integer-token-p d))))))

  
(defun _read (stm eof-error-p eof-value recursive-p pres-white &optional can-dot can-rpar)
  (do ()
      (nil)
    (let ((ch (peek-char t stm nil)))
      (if ch
        (let ((typ (_char-type ch)))
          (cond ((memq typ '(:constituent :sescape :mescape))
                  (return (let ((x (_read-token-list stm pres-white)))
                            (if (_dots x)
                              (if (and can-dot (null (cdr x))) _dot_ex
                                                               (error 'reader-error))
                              (if (null *read-suppress*)
                                (if (_maybe-number x)
                                  (or (_integer-token-p x)
                                      (_ratio-token-p x)
                                      (_float-token-p x)
                                      (error x))
                                  (let ((p (_member-trait _package x)))
                                    (if p (cond ((eq x p) (intern (_chain-s (cdr x)) *keyword-package*))
                                                ((eq (cdr p) (_member-trait _package (cdr p)))
                                                  (intern (_chain-s (cddr p)) (_get-package (_chain-s x p))))
                                                ((multiple-value-bind (sym st)
                                                                      (find-symbol (_chain-s (cdr p)) (_get-package (_chain-s x p)))
                                                  (if (eq st :external) sym (err)))))
                                          (intern (_chain-s x))))))))))
                ((and (eq ch #\)) can-rpar) (read-char stm) (return _rpar)) ;!!! EQL must be for chars
                ((functionp typ) (let ((r (multiple-value-list (funcall typ stm (read-char stm)))))
                                   (if r (return (car r)))))
                ((error 'reader-error))))
        (if (or eof-error-p recursive-p) (error 'end-of-file)
                                         (return eof-value))))))


(defun read (&optional stm (eof-error-p t) eof-value recursive-p)
  (_read stm eof-error-p eof-value recursive-p nil))
  
(defun read-preserving-whitespace (&optional stm (eof-error-p t) eof-value recursive-p)
  (_read stm eof-error-p eof-value recursive-p t))
|#

(defun _rpar-reader (stm ch)
		(declare (ignore stm ch))
  (error "an object cannot start with #\\)"))

(defun _quote-reader (stm ch)
		(declare (ignore ch))
  (list 'quote (read stm t nil t)))
  
(defun _semicolon-reader (stm ch)
		(declare (ignore ch))
  (do ()
      ((eql (read-char stm nil #\Newline t) #\Newline)))
  (values))

(defun _dquote-reader (stm ch)
  (do (p
       (x (read-char stm) (read-char stm)))
      ((eql x ch) (_rev-list-to-string p))
    (push (if (eq (_char-type x) :sescape)
            (read-char stm)
            x)
          p)))

#|

(defun _dquote-reader (stm ch)
  (do ((v (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
       (x (read-char stm) (read-char stm)))
      ((eql x ch) v)
    (vector-push-extend
      (if (eq (_char-type x) :sescape)
        (read-char stm)
        x)
      v)))
|#


(setf (readtable-case *readtable*) :upcase)

(defun _token-as-string (token)
  (ext:string-concat (mapcar #'car token)))

#|!!!  
(defun _read-rational (stm radix &optional ch)
  (let* ((a (_read-simple-token stm t))) ;!!!
    (or (_integer-token-p a radix)
        (_ratio-token-p a radix)
        (if ch (_err 'reader-error E_LISP_IsNotRational (_token-as-string a) ch radix)
               (error 'reader-error)))))
|#               


(setq *readtable* (copy-readtable nil))

(_load "defmacro")
        


(defun terpri (&optional stm)
  (write-char #\Newline stm)
  nil)
  
(defun fresh-line (&optional stm)
  (unless (eql (line-position stm) 0)
    (terpri stm)
    t))

(defmacro with-open-file ((stream &rest file-args) &rest body)
  `(let ((,stream (open ,@file-args)))
     (unwind-protect
       (progn ,@body)
       (close ,stream))))
       
(defun _cased-string (s case)
  (case case
    (:local s)
    (:common (cond ((every (lambda (ch)
                             (or (upper-case-p ch) (not (both-case-p ch))))
                           s)
                    	(string-downcase s))
				   ((every (lambda (ch)
                             (or (lower-case-p ch) (not (both-case-p ch))))
                           s)
                    	(string-upcase s))
                   (t s)))
    (t (error 'type-error :datum case :expected-type '(or (eql :local) (eql :common))))))

(defun _pathname-field (fn pn case)
  (let ((c (_pathname-field-ex fn (pathname pn))))
    (cond ((stringp c) (_cased-string c case))
          ((listp c) (mapcar
                       (lambda (x)
                         (if (stringp x) (_cased-string x case) x))
                       c))
          (t c))))

(defun pathname-host (pn &key (case :local))
  (_pathname-field 0 pn case))

(defun pathname-device (pn &key (case :local))
  (_pathname-field 1 pn case))

(defun pathname-directory (pn &key (case :local))
  (_pathname-field 2 pn case))

(defun pathname-name (pn &key (case :local))
  (_pathname-field 3 pn case))

(defun pathname-type (pn &key (case :local))
  (_pathname-field 4 pn case))

(defun pathname-version (pn)
  (_pathname-field-ex 5 (pathname pn)))      
  
#|!!!R     
(defun make-pathname (&key host device directory (name nil c-name) (type nil c-type) version (defaults *default-pathname-defaults*) case)
  (_make-path-name (or host (pathname-host defaults))
  								 (or device (pathname-device defaults))  								 
  								 (if (and (listp directory) (eq (car directory) :relative) (consp (pathname-directory defaults)))
  								   (append (pathname-directory defaults) (cdr directory))
  								   (or directory (pathname-directory defaults)))  								 
  								 (if c-name name (pathname-name defaults))
  								 (if c-type type (pathname-type defaults))
  								 (or version (pathname-version defaults))))
|#

(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) (def-ver :newest))
  (setq pathname (pathname pathname)
        defaults (pathname defaults))
  (let* ((host      (pathname-host pathname))
         (device    (pathname-device pathname))
         (directory (pathname-directory pathname))
         (name      (pathname-name pathname))
         (type      (pathname-type pathname))
         (version   (or (pathname-version pathname) def-ver)))
    (if (eq (car directory) :relative)
      (let ((def-dir (pathname-directory defaults)))
        (when (consp def-dir)
           (setq directory (append def-dir (cdr directory)))
           (do* ((p directory)
                 (x (car p) (car p))) 
                   ((null p))
             (setq p (if (and (or (stringp x) (eq x :wild))
                              (eq (cadr p) :back))
                       (setq directory (append (ldiff directory p)
                                               (cddr p)))
                       (cdr p)))))))
    (apply #'make-pathname :defaults defaults
                           (append (if host      (list :host      host))
                                   (if device    (list :device    device))
                                   (if directory (list :directory directory))
                                   (if name      (list :name      name))
                                   (if type      (list :type      type))
                                   (if version   (list :version   version))))))
                 
	
(defun host-namestring (pn)
	(let ((h (pathname-host pn)))
		(cond ((stringp h) h)
			  ((consp h) (ext:string-concat (first h) "://" (second h)))
			  (t ""))))

(defun file-namestring (pn)
  (let ((s (pathname-name pn))
        (ext (pathname-type pn)))
    (if ext (ext:string-concat s "." ext)
            s)))

(defun directory-namestring (pn &aux r)
  (let* ((dev (pathname-device pn))
		(dir (pathname-directory pn))
		(lp (typep pn 'logical-pathname))
		(sep (if lp ";"
                    (if (and (member :WIN32 *features*) (not (consp (pathname-host pn)))) "\\" "/"))))
	(push (if (or (null dev) (eq dev :unspecific)) "" (ext:string-concat dev ":")) r)
	(when dir
	  (if (and lp (eq (car dir) :relative))
		(push ";" r))
	  (if (and (not lp) (eq (car dir) :absolute))
		(push sep r))
	  (dolist (x (cdr dir))
        (case x
		  ((:up :back) (push ".." r))
		  (:wild (push "*" r))
		  (:wild-inferiors (push "**" r))
		  (t (push x r)))
		(push sep r))))
  (apply #'ext:string-concat (nreverse r)))

(defun namestring (pn)
	(let ((r "")
		  (dev (pathname-device pn))
		  (h (host-namestring pn))
		  (hs (stringp (pathname-host pn))))
		(if (or (null dev) (eq dev :unspecific))
		  (if (or (not hs) (equal h ""))
            (setq r h)
			(setq r (if (member :WIN32 *features*) (ext:string-concat "\\\\" h) (ext:string-concat h ":")))))
		(ext:string-concat r (directory-namestring pn) (file-namestring pn))))
 

(defvar *logical-pathname-translations* (make-hash-table :test #'equalp))


(defconstant _eof '(eof))	

(defvar *source-file-types* '("lisp"))
(defvar *compiled-file-types* '("fas"))


#|!!!
(defun %put (s k v)
	(setf (get s k) v))
|#	

(defun remprop (s i)
  (multiple-value-bind (r removed-p) (%remf (symbol-plist s) i)
    (when (and removed-p (atom r))
      (%putplist s r))
    removed-p))
    
#|!!!hardwird           
(defun proclaim (spec)
  (case (car spec)
    (special (_proclaim-special (cdr spec)))
    ((inline notinline) (dolist (x (cdr spec))
                          (%put (get-funname-symbol x) 'inlinable (car spec))))))
|#	
	
    

;!!! because bug in our defmacro, REPEAT
(sys::%putd 'defmacro
  (sys::make-macro
    (function defmacro (lambda (form env)
      (declare (ignore env))
      (multiple-value-bind (expansion expansion-lambdabody name lambdalist docstring)
          (sys::make-macro-expansion (cdr form) form)
        (declare (ignore expansion-lambdabody lambdalist))
        `(LET ()
           (EVAL-WHEN (COMPILE LOAD EVAL)
             (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
             ,@(if docstring
                 `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                 '())
             (SYSTEM::%PUTD ',name (SYSTEM::MAKE-MACRO ,expansion)))
           (EVAL-WHEN (EVAL)
             (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION
                           (CONS ',form (THE-ENVIRONMENT))))
           ',name))))))
           
        
        
        
        
(defvar *toplevel-environment* (eval '(the-environment)))
(defvar *toplevel-denv* (svref *toplevel-environment* 4))
;(defvar *toplevel-denv* nil)

(defmacro defun (&whole form name lambdalist &body body &environment env)
  (multiple-value-bind (body-rest declarations docstring) (parse-body body t)
    (let ((symbolform
           (if (atom name)
             `',name
             `(LOAD-TIME-VALUE (GET-SETF-SYMBOL ',(second name)))))
          (lambdabody `(,lambdalist
                        (DECLARE (SYS::IN-DEFUN ,name) ,@declarations)
                        (BLOCK ,(function-block-name name)
                          ,@body-rest))))
      `(LET ()
         (SYSTEM::REMOVE-OLD-DEFINITIONS ,symbolform)
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
                        (FUNCTION ,name (LAMBDA ,@lambdabody)))
         (EVAL-WHEN (EVAL)
           (SYSTEM::%PUT ,symbolform 'SYSTEM::DEFINITION
                         (CONS ',form (THE-ENVIRONMENT))))
         ',name))))

    

(defun list-nreverse (x)
  (nreverse x))      

(defun logical-pathname-p (pn)
	(eq (type-of pn) 'logical-pathname))
  

(_load "sort")

(defun map (result-type function sequence &rest more-sequences)
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences))))
    (if (null result-type)
        (do ((i 0 (1+ i))
             (l l))
            ((>= i l) nil)
          (declare (fixnum i l))
          (apply function (mapcar #'(lambda (z) (elt z i))
                                  more-sequences)))
        (let ((x (make-sequence result-type l)))
          (do ((i 0 (1+ i))
               (l l))
              ((>= i l) x)
            (declare (fixnum i l))
            (setf (elt x i)
                  (apply function (mapcar #'(lambda (z) (elt z i))
                                          more-sequences))))))))	   
              

(defvar *print-array*           t)
(defvar *print-base*            10)
(defvar *print-case*            :upcase)
(defvar *print-circle*          nil)
(defvar *print-escape*          t)
(defvar *print-gensym*          t)
(defvar *print-length*          nil)
(defvar *print-level*           nil)
(defvar *print-lines*           nil)
(defvar *print-miser-width*     nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-pretty*          nil)
(defvar *print-radix*           nil) 
(defvar *print-readably*        nil)
(defvar *print-right-margin*    nil)


(defvar *print-circle-table*)
(defvar *prin-stream*)
(defvar *prin-level*)
(defvar *prin-bqlevel*)

(_load "print-builtin")
  


#|  !!!
           
(defun _get-integer (i radix &aux p)
  (do ()
      (nil)
		(multiple-value-bind (q r) (truncate i radix)
			(push (digit-char r radix) p)
			(if (zerop q) (return p))	
			(setq i q))))
|#			
			
#|!!!
(defun _char-name (ch)
  (with-output-to-string (stm)
    (princ (char-code ch) stm)))
|#


(defun princ (x &optional stm)
  (write x :stream stm :escape nil :readably nil))


(defun _wild-p (x &optional isdir)
	(if (consp x)
		(dolist (a x nil)
			(if (or (eq a :wild)
					(and isdir (eq a :wild-inferiors))
					(and (stringp a) (or (find #\? a) (find #\* a))))
			  (return t)))
		(eq x :wild)))

(defun wild-pathname-p (pn &optional fk)
  (ecase fk
    ((nil) (or (wild-pathname-p pn :host)
               (wild-pathname-p pn :device)
               (wild-pathname-p pn :directory)
               (wild-pathname-p pn :name)
               (wild-pathname-p pn :type)
               (wild-pathname-p pn :version)))
    (:host      (_wild-p (pathname-host pn)))
    (:device    (_wild-p (pathname-device pn)))           
    (:directory (_wild-p (pathname-directory pn) t))   ;!!! check subdirs
    (:name      (_wild-p (pathname-name pn)))
    (:type      (_wild-p (pathname-type pn)))
    (:version   (_wild-p (pathname-version pn)))))


#|
;; for the time being the files don't have to be searched:
(defun search-file (filename extensions)
  (mapcan #'(lambda (extension)
              (let ((filename (merge-pathnames filename
                                     (make-pathname :type extension))))
                (if (probe-file filename) (list filename) '())))
          extensions))
|#          
          


;; A piece of "DO-WHAT-I-MEAN":
;; Searches for a program file.
;; We search in the current directory and then in the directories
;; listed in *load-paths*.
;; If an extension is specified in the filename, we search only for
;; files with this extension. If no extension is specified, we search
;; only for files with an extension from the given list.
;; The return value is a list of all matching files from the first directory
;; containing any matching file, sorted according to decreasing FILE-WRITE-DATE
;; (i.e. from new to old), or NIL if no matching file was found.


(defun ppn-fwd (f keep-dirs)    ; probe-pathname + file-write-date
  (multiple-value-bind (true-name phys-name fwd) (probe-pathname f)
    (when (and true-name (or keep-dirs (pathname-name true-name)))
      phys-name)))


(defun search-file (filename &optional extensions (keep-dirs t))
  ;; merge in the defaults:
;!!!R  (setq filename (merge-pathnames filename "*.*"))

;  (sys::_pr "--------------search-file-----" filename extensions)

  (let* ((already-searched nil)
         (path-nonW (pathname filename))
         (path-wild (if (pathname-name path-nonW)
                        (merge-pathnames path-nonW "*.*")
                        ;; do not append *.* to directories
                        path-nonW))
         (use-extensions (null (pathname-type path-nonW))))


    (dolist (dir (cons '""
                       ;; when filename has "..", ignore *load-paths*
                       ;; (to avoid errors with "**/../foo"):
                       (if (memq :up (pathname-directory path-nonW))
                           '()
                           (mapcar #'pathname *load-paths*))))
      (let* ((wild-p (wild-pathname-p dir))
			 (search-filename (merge-pathnames (if wild-p path-wild path-nonW) dir)))
;        (_pr "---search-filename=" search-filename dir *load-paths*)
        (unless (member search-filename already-searched :test #'equal)
          (let ((xpathnames (if wild-p
                              (nconc (directory search-filename :full t :circle t :if-does-not-exist :ignore)
                                     (directory (make-pathname :type nil :defaults search-filename)))
                              (let ((f (ppn-fwd search-filename keep-dirs))
                                    (e (and use-extensions extensions
                                         (mapcan #'(lambda (ext)
                                               (let ((f (ppn-fwd (make-pathname :type ext :defaults search-filename) keep-dirs)))
                                                 (and f (list f))))
                                            extensions))))
                                (if f (cons f e) e)))))
            (when wild-p
              (when (and use-extensions extensions) ; filter the extensions
                (setq xpathnames
                      (delete-if-not
                       #'(lambda (xpathname)
                           (let ((ext (pathname-type xpathname)))
                             (or (null ext) ; no extension - good!
                                 (member ext extensions
                                         :test #+WIN32 #'string-equal
                                               #-WIN32 #'string=))))
                       xpathnames))))


	            (when xpathnames
    	          ;; reverse sort by date:
;        	      (_pr "-----before sort:" xpathnames)
            	  (return (sort xpathnames #'> :key #'file-write-date))))
          (push search-filename already-searched))))))

          
;; preliminary; needed here for open-for-load
(defun warn (&rest args) (print (cons 'warn args)) nil)

(defun open-for-load (filename extra-file-types external-format &aux stream (present-files t) obj path)
		(declare (ignore external-format))
  (if (streamp filename)
    (values filename filename)
		(labels ((compiledp (name)
							 (member (pathname-type name) *compiled-file-types*
											 :test #'string=))
				 (my-open (name)
;				 	(_pr "-----my-open---" name)
					 (open name :direction :input-immutable :element-type 'character
										 #+UNICODE :external-format
										 #+UNICODE (if (compiledp name)
														charset:utf-8
														external-format)
										 :if-does-not-exist nil))
				 (bad (error-p stream message)
					(close stream)
					(when (eq *load-obsolete-action* :delete)
						(delete-file stream))
					(if error-p
									 (error-of-type 'file-error :pathname (pathname stream)
																	message 'load (pathname stream))
									 (warn message 'load (pathname stream))))
				 (check-compiled-file (stream last-p obj)
					(and (or (and (consp obj) (eq (car obj) 'system::version))
							(bad last-p stream (TEXT "~s: compiled file ~s lacks a version marker")))
						(or (= 2 (length obj))
							(bad last-p stream (TEXT "~s: compiled file ~s has a corrupt version marker ~s")))
						(or (equal (system::version) (eval (second obj)))
								(bad last-p stream (TEXT "~s: compiled file ~s has an older version marker"))))))
			(setq
				filename (pathname filename)
			    path filename
           		stream (my-open path))
;			(_pr "============path=" path)
			(do ()
				((and stream
				      (or (not (compiledp stream))
						  (check-compiled-file
				 			stream (or (eq *load-obsolete-action* :error)
							 (eq present-files t)
							 (cdr present-files))
							(setq obj (read stream)))))
					(values stream path))
;      			(_pr "----open-for-load-----" present-files)
				
				(when (eq present-files t)
					;; File with precisely this name not present OR bad
					;; Search among the files the most recent one
					;; with the same name and the Extensions "LISP", "FAS":
					(setq present-files (search-file filename (append extra-file-types *compiled-file-types* *source-file-types*) nil)))
				(if present-files
				  (progn (setq path (pop present-files)
				                    stream (my-open path)))
					(if stream
						;; bad compiled STREAM, nowhere else to look ==> die
						(check-compiled-file stream t obj)
						;; no file - return NIL
						(return-from open-for-load (values nil filename))))))))
      
(defvar *load-level* 0)      
(defvar *load-echo* nil)
(defvar *load-compiling* nil)
(defvar *load-obsolete-action* nil)



(defun load (filespec &key ((:verbose *load-verbose*) *load-verbose*)
                           ((:print *load-print*) *load-print*)
                           ((:echo *load-echo*) *load-echo*)
                           ((:compiling *load-compiling*) *load-compiling* c-top)
                           (if-does-not-exist t) (external-format :default))
		(declare (ignore c-top))
                           
;!!!  (_pr "-----load----*load-paths*=" *load-paths*) ;!!!!
                           
  (multiple-value-bind (stm filename) (open-for-load filespec nil external-format)
;    (_pr "---after open-for-load---stream=")
;    (_pr stm)
		(unless stm
			(if if-does-not-exist
					(error-of-type 'file-error
						:pathname filename
						(TEXT "~S: A file with name ~A does not exist")
						'load filename)
					(return-from load nil)))
		(let* ((*readtable* *readtable*)
			    (*package* *package*)
			    (*load-level* (1+ *load-level*))
				(indent (if (null *load-verbose*) ""
				(make-string *load-level* :initial-element #\Space)))
				(*load-pathname* (if (pathnamep filename) (merge-pathnames filename) nil))
				(*load-truename* (if (pathnamep filename) (truename filename) nil))
				(*current-source-file* *load-truename*)
				(compiling (and *load-compiling* (memq :compiler *features*)))
;				(*default-pathname-defaults* (make-pathname :host (pathname-host *load-pathname*)
;					                                             :device (pathname-device *load-pathname*)
;					                                             :directory (pathname-directory *load-pathname*)))
			  )
		 	(when *load-verbose*
				(fresh-line)
				(write-string ";;")
				(write-string indent)
				(write-string (TEXT "Loading file "))
		        (princ filespec)
				(write-string " ...")
				(fresh-line))
      (if compiling
        (compiler::c-reset-globals))
     	(unwind-protect					 
				(do ()
						(nil)
					(let ((e (read stm nil _eof)))
						(if (eq e _eof) (return))
						(let ((v (multiple-value-list
                          (cond ((compiled-function-p e) (funcall e))
                                (compiling (funcall (compile-form-in-toplevel-environment e)))
                                (t (eval e))))))
              (when *load-print* (when v (_pr (car v))))))) ;;!!! must be print
				(sys::built-in-stream-close stm)
        (if compiling
				  (compiler::c-report-problems)))
			(when *load-verbose*
				(fresh-line)
				(write-string ";;")
 				(write-string indent)    
				(write-string "Loaded file ")
				(princ filespec)
				(fresh-line))))
  t)


(defun _eval-string (str)
  (ignore-errors
		(with-output-to-string (ostm)
			(with-input-from-string (istm str)
				(prin1 (eval (read istm)) ostm)))))

(defun _clear-definitions ()
  (do-all-symbols (sym) 
  	(remprop sym 'sys::definition)
  	(when (and (fboundp 'clos::install-dispatch)
               (fboundp sym)
               (clos::generic-function-p (symbol-function sym)))
      (let ((gf (symbol-function sym)))
        (when (clos::gf-never-called-p gf)
          (clos::install-dispatch gf)))))
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
)
			

        
(defun keyword-test (arglist kwlist)
;  (_pr "-------keyword-test-------" arglist kwlist)
  (unless (eq kwlist t)
		(let ((unallowed-arglistr nil)
					(allow-other-keys-flag nil))
			(do ((arglistr arglist (cddr arglistr)))
					((null arglistr))
				(if (eq (first arglistr) ':ALLOW-OTHER-KEYS)
						(if (second arglistr) (setq allow-other-keys-flag t))
						(do ((kw (first arglistr))
								 (kwlistr kwlist (cdr kwlistr)))
								((or (null kwlistr) (eq kw (first kwlistr)))
								 (if (and (null kwlistr) (null unallowed-arglistr))
										 (setq unallowed-arglistr arglistr))))))
			(unless allow-other-keys-flag
				(if unallowed-arglistr
					(cerror (TEXT "Both will be ignored.")
									(TEXT "Invalid keyword-value-pair: ~S ~S")
									(first unallowed-arglistr) (second unallowed-arglistr)))))))
                
        

                      
#|!!!	
(defmacro with-input-from-string 	((var string) &body body)
	`(LET ((,var (MAKE-STRING-INPUT-STREAM ,string)))
       (UNWIND-PROTECT
         (PROGN ,@body)
         (CLOSE ,var))))
|#         

(defmacro with-input-from-string
    ((var string &key (index nil sindex) (start '0 sstart) (end 'NIL send))
     &body body &environment env)
		(declare (ignore env))
  (multiple-value-bind (body-rest declarations)
      (SYSTEM::PARSE-BODY body)
    `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string
                   ,@(if (or sstart send)
                       `(,start ,@(if send `(,end) '()))
                       '()))))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         ,@(if sindex
               `((SETF ,index (SYSTEM::STRING-INPUT-STREAM-INDEX ,var)))
                '())
         (CLOSE ,var)))))

;(load "seq")
(load "cl")
(load "pathname")
;(load 'ext)

(load "errorcodes")




(defconstant *common-lisp-user-package* (find-package "COMMON-LISP-USER"))

;(load "clos")
;(load "struct")
;(load "compiler")
;(load "cond")
(load "macros")
;(load "debug")
;(load "env")

(defun princ-to-string (x)
  (with-output-to-string (stm)
    (princ x stm)))

(defun prin1-to-string (x)
  (with-output-to-string (stm)
    (prin1 x stm)))

(defun write-to-string (x &rest rest) ;!!! &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin)
  ;; without macro with-output-to-string because #'close not defined
  (let ((stm (make-string-output-stream)))
    (apply #'write x :stream stm rest)
    (get-output-stream-string stm)))


(load "break")

(load "win32")



(defvar *_opt-loaded* nil)
;(if (_full) (load "opt"))

;(unless sys::*_opt-loaded* (load 'opt))

(load "string")

(load "reader")

(load "math")


(load "clisp")

(unless (memq :NO-HUGE *features*)
  (load "debug"))

;!!!(_clear-definitions)

(in-package "EXT")

(defvar *for-execute* nil)

(defun make-exe (filename stubpath destpath)
	(multiple-value-bind (fasfile warn-p fail-p) (compile-file filename)
				(declare (ignore warn-p))			
		(when fail-p
			(return-from make-exe nil))
		(with-open-file (stm fasfile :direction :input)
		 	(do (r)
            		(nil)
				(let ((f (read stm nil sys::_eof)))
					(if (eq f sys::_eof)
						(return (setq *for-execute* (nreverse r)))
						(push f r))))))

  	(with-open-file (dstm destpath :direction :output :element-type '(integer 0 255))
    	(with-open-file (stubstm stubpath :direction :input :element-type '(integer 0 255))
      		(let ((ar (make-sequence 'vector (file-length stubstm))))
        		(read-sequence ar stubstm)
        		(write-sequence ar dstm)))
    	(write-sequence (map 'vector #'char-code "$L$I$S$P") dstm)
    	(sys::savemem dstm))
  	(format t "compiled to ~A" destpath)
  	(setq *for-execute* nil)
	t)

(defun run-for-execute ()
  (do ()
        ((null *for-execute*))
    (let ((e (pop *for-execute*)))
      (cond ((compiled-function-p e) (funcall e))
             (t (eval e))))))


#+CFFI-SYS (load "cffi-sys")


(in-package "CL-USER")  
  
;(compile '_integer-token-p)




