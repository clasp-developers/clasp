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

;;;  Important macros, loaded 2 times (before & after of expander)

(eval-when (compile load eval)
  (setq *package* (find-package "SYS")))

(%putd 'when (make-macro
             (function when (lambda (form env)
								(declare (ignore env))
                              (let ((c (cadr form))
                                    (r (cddr form)))
                                `(if ,c (progn ,@r)))))))
                                
(%putd 'unless (make-macro
             (function unless (lambda (form env)
								(declare (ignore env))
                                (let ((c (cadr form))
                                      (r (cddr form)))
                                  `(if ,c nil (progn ,@r)))))))
                                  
(%putd 'defconstant (make-macro
   (function defconstant (lambda (form env)
		(declare (ignore env))
     (let ((n (cadr form))
           (v (caddr form))
           (d (cadddr form))
           (d-p (cdddr form)))
       `(progn (%proclaim-constant ',n ,v)
               ,(if d-p `(setf (documentation ',n 'variable) ',d))
               ',n))))))
               
(defun complement (func)
  #'(lambda (&rest r)
      (not (apply func r))))

(defun constantly (val)
  #'(lambda (&rest r)
		(declare (ignore r))
      val))

(defun _key (key)
  (or key #'identity))
          
(defun _end (end seq)
	(or end (length seq)))
					
(defun _setf-name-p (n)
  (and (consp n) (eq (car n) 'setf)))

(defun _test (item test test-not)
  (if test-not #'(lambda (z) (not (funcall test-not item z)))
               #'(lambda (z) (funcall (or test #'eql) item z))))

#|
(defun _test-if (p)
  #'(lambda (z)
      (funcall p z)))

(defun _test-if-not (p)
  #'(lambda (z)
      (not (funcall p z))))
|#

(defun _list-test (mc g list key)
  (if list (if (funcall g (funcall key (funcall mc (car list))))
             list
             (_list-test mc g (cdr list) key))))

(defun member-if (p list &key key)
  (_list-test #'identity p list (_key key)))

(defun member-if-not (p list &key key)
  (member-if (complement p) list :key key))
  
(%putd 'lambda (make-macro
             (function lambda (lambda (form env)
								(declare (ignore env))
                                (let ((bvl-decls-and-body (cdr form)))
									(declare (ignore bvl-decls-and-body))
                                  (list 'function form))))))

(defun _get-package (name)
  (let ((p (find-package name)))
    (if p p (error 'package-error :package (string name)))))


(defun setf-symbol (symbol)
  (make-symbol (ext:string-concat "(SETF "
                       (let ((pack (symbol-package symbol)))
                         (if pack (package-name pack)
                                  "#"))
                       ":"
                       (symbol-name symbol)
                       ")")))
                       
(defun _subforms (x)
  (if x (cons (gensym) (_subforms (cdr x)))))
  
(defun _gensyms (n)
	(if (zerop n) nil
								(cons (gensym) (_gensyms (1- n)))))
								
(defun _concs (&rest r)
  (intern (apply #'ext:string-concat r)))

(defconstant t 't)

(defconstant *keyword-package* (find-package 'keyword))

(defun _lambda-list-keyword-p (x)
  (memq x lambda-list-keywords))
  
;!!! (defconstant %class-structure-types '(class structure-object))

(defun _try-aux (a r)
  (cond ((and (null r) (null a)) nil)
        ((eq a '&aux) r)
        ((err))))

(defun _find-key (k args &optional def)
  (let* ((d '(:absent))
         (v (getf args k d)))
    (if (eq v d) def
                 v)))

(defun _bind-key (lam args)
  (if lam 
    (let ((a (car lam))
          (r (cdr lam)))
      (cond ((_lambda-list-keyword-p a) (if (eq a '&allow-other-keys) (_try-aux (car r) (cdr r))
                                                                      (_try-aux a r)))
            ((consp a) (let ((k (intern (symbol-name (car a)) *keyword-package*)))
                          (cons (list (car a) `(_find-key ,k ,args ,(cadr a))) (_bind-key r args))))
            (t (let ((k (intern (symbol-name a) *keyword-package*)))
                 (cons (list a `(_find-key ,k ,args)) (_bind-key r args))))))))  

(defun _try-key (a r args)
  (if (eq a '&environment)
    (cons (list (car r) '_e) (_try-key (cadr r) (cddr r) args))
    (if (eq a '&key) (_bind-key r args)
                     (_try-aux a r))))

(defun _bind-rest (lam args)
  (cons (list (car lam) args) (_try-key (cadr lam) (cddr lam) args)))

(defun _try-rest (a r args)
  (if (memq a '(&rest &body)) (_bind-rest r args)
                               (_try-key a r args)))

(defun _bind-optional (lam args)
  (if lam
    (let ((a (car lam))
          (r (cdr lam)))
      (cond ((consp a) (let ((n (car a))
                             (d (cadr a))
                             (p (cddr a)))
                         (if p (list* (list n `(if ,args (car ,args) ,d)) (list (car p) args) (_bind-optional r `(cdr ,args)))
                               (cons  (list n `(if ,args (car ,args) ,d))                     (_bind-optional r `(cdr ,args))))))
						((_lambda-list-keyword-p a) (_try-rest a r args))
									(t (cons (list a `(car ,args)) (_bind-optional r `(cdr ,args))))))))

(defun _try-optional (a r args)
  (if (eq a '&optional) (_bind-optional r args)
                        (_try-rest a r args)))

(defun _macro-lam-list (lam whole &optional (args '_args))
  (if lam (let ((a (car lam))
                (r (cdr lam)))
            (cond ((consp a) (append (_macro-lam-list a a `(car ,args)) (_macro-lam-list r whole `(cdr ,args))))
                  ((_lambda-list-keyword-p a) (cond ((eq a '&whole)
                                                    (cons (list (car r) whole) (_macro-lam-list (cdr r) whole)))
                                                  ((eq a '&environment)
                                                    (cons (list (car r) '_e) (_macro-lam-list (cdr r) whole)))
                                                  ((_try-optional a r args))))
                  (t (cons (list a `(car ,args))
                           (if (listp r) (_macro-lam-list r whole `(cdr ,args))
                                         (cons (list r `(cdr ,args)) nil))))))))

(defun _make-macro-expansion (macrodef)
  (let ((name (car macrodef))
        (lam (cadr macrodef))
        (forms (cddr macrodef)))
    `(lambda (_f _e &aux (_whole _f) (_args (cdr _f)) ,@(_macro-lam-list lam '_f))
       (block ,name ,@forms))))

(%putd 'defmacro (make-macro
	(function defmacro (lambda (form env)
	(declare (ignore env))
    (let* ((macrodef (cdr form))
           (name (car macrodef)))
      (if (symbolp name)
        `(progn (%putd ',name (make-macro ,(_make-macro-expansion macrodef)))
                ',name)
        (error "Invalid defmacro")))))))

(defmacro loop (&body body)
  (let ((tag (gensym)))
		`(BLOCK NIL (TAGBODY ,tag ,@body (GO ,tag)))))

(defun _prog (a body op env)
	(declare (ignore env))
	(multiple-value-bind (forms decls)
			(parse-body body)
		(list 'block nil (list op a (cons 'declare decls) (cons 'tagbody forms)))))

#|!!!  
(defun _prog (a r op)
  `(block nil (,op ,a ,@(_decls r) (tagbody ,@(_body r)))))
  |#

(defmacro prog (a &body body &environment env)
  (_prog a body 'let env))

(defmacro prog* (a &rest body &environment env)
  (_prog a body 'let* env))

(defun _dodecls (a)
  (if a (cons (let ((d (car a)))
                (if (atom d) d
                             (list (car d) (cadr d))))
              (_dodecls (cdr a)))))

(defun _dosteps (a)
  (if a (let ((r (_dosteps (cdr a)))
              (d (car a)))
          (if (and (consp d) (cddr d)) (list* (car d) (caddr d) r)
                                       r))))

(defun _do (vars test r body op setop env)
	(declare (ignore env))
  (let ((_lab (gensym)))
		(multiple-value-bind (forms decls)
				(parse-body body)		
			(list 'block nil (list op (_dodecls vars) (cons 'declare decls)
			                       (list 'tagbody
			                       			 _lab
			                       			 (list 'if test (list 'return (cons 'progn r)))
			                       			 (cons 'tagbody forms)
			                       			 (cons setop (_dosteps vars))
						                       (list 'go _lab)))))))

(defmacro do (vars (test &rest r) &body body &environment env)
  (_do vars test r body 'let 'psetq env))

(defmacro do* (vars (test &rest r) &body body &environment env)
  (_do vars test r body 'let* 'setq env))
  
(defmacro ecase (expr &body clauses)
  (list* 'case expr (append clauses '((ecase-error-flag)))))


               
#|          

                                    
(_putd-macro 'ecase
             (function ecase (lambda (form env)
                               (let ((expr (cadr form))
                                     (clauses (cddr form)))
                                 (list* 'case expr (append clauses '((ecase-error-flag))))))))
|#                                 

  
(defmacro prog1 (x &rest r)
  `(values (multiple-value-prog1 ,@(cons x r))))

(defmacro prog2 (x y &rest r)
  `(progn ,x (prog1 ,@(cons y r))))

;; returns the name of the implicit block for a function-name
(defun function-block-name (funname)
  (if (atom funname) funname (second funname)))

;; inserts an implicit BLOCK in the BODY.
;; uses *VENV* and *FENV*.
(defun add-implicit-block (name lb)
;  (_pr "----------------add-implicit-block-------------")
;  (_pr name lb)
  (cons (car lb)
				(multiple-value-bind (body-rest declarations docstring)
						(sys::parse-body (cdr lb) t)
					(append (if declarations (list (cons 'DECLARE declarations)))
									(if docstring (list docstring))
									(list (list* 'BLOCK (function-block-name name) body-rest))))))



            
