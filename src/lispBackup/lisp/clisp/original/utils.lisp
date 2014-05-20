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

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))


;;; Collect  --  Public
;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
	(error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))








(defun apropos-search (symbol string)
  (declare (simple-string string))
  (do* ((index 0 (1+ index))
	(name (symbol-name symbol))
	(length (length string))
	(terminus (- (length name) length)))
       ((> index terminus)
	nil)
    (declare (simple-string name)
	     (type index index terminus length))
    (if (do ((jndex 0 (1+ jndex))
	     (kndex index (1+ kndex)))
	    ((= jndex length)
	     t)
	  (declare (fixnum jndex kndex))
	  (let ((char (schar name kndex)))
	    (unless (char= (schar string jndex) (char-upcase char))
	      (return nil))))
	(return t))))


;;; MAP-APROPOS -- public (extension).
;;;
(defun map-apropos (fun string &optional package external-only)
  "Call FUN with each symbol that contains STRING.
  If PACKAGE is supplied then only use symbols present in
  that package.  If EXTERNAL-ONLY is true then only use
  symbols exported from the specified package."
  (let ((string (string-upcase string)))
    (declare (simple-string string))
    (flet ((apropos-in-package (package)
             (if external-only
                 (do-external-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol)))
                 (do-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol))))))
      (if (null package)
          (mapc #'apropos-in-package (list-all-packages))
          (apropos-in-package (package-or-lose package))))
    nil))

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (write-string ", value: ")
    (prin1 (symbol-value symbol)))
  (if (fboundp symbol)
      (write-string " (defined)")))



;;; APROPOS -- public.
;;; 
(defun apropos (string &optional package external-only)
  "Briefly describe all symbols which contain the specified String.
  If Package is supplied then only describe symbols present in
  that package.  If External-Only is true then only describe
  external symbols in the specified package."
  (map-apropos #'briefly-describe-symbol string package external-only)
  (values))

;;; APROPOS-LIST -- public.
;;; 
(defun apropos-list (string &optional package external-only)
  "Identical to Apropos, except that it returns a list of the symbols
  found instead of describing them."
  (collect ((result))
    (map-apropos #'(lambda (symbol)
		     (result symbol))
		 string package external-only)
    (result)))


;;;; WITH-PACKAGE-ITERATOR

(defmacro with-package-iterator ((mname package-list &rest symbol-types)
				 &body body)
  (let* ((packages (gensym))
	 (these-packages (gensym))
	 (ordered-types (let ((res nil))
			  (dolist (kind '(:inherited :external :internal)
					res)
			    (when (member kind symbol-types)
			      (push kind res)))))  ; Order symbol-types.
	 (counter (gensym))
	 (kind (gensym))
	 (hash-vector (gensym))
	 (vector (gensym))
	 (package-use-list (gensym))
	 (init-macro (gensym))
	 (end-test-macro (gensym))
	 (real-symbol-p (gensym))
	 (BLOCK (gensym)))
    `(let* ((,these-packages ,package-list)
	    (,packages `,(mapcar #'(lambda (package)
				     (if (packagep package)
					 package
					 (find-package package)))
				 (if (consp ,these-packages)
				     ,these-packages
				     (list ,these-packages))))
	    (,counter nil)
	    (,kind (car ,packages))
	    (,hash-vector nil)
	    (,vector nil)
	    (,package-use-list nil))
       ,(if (member :inherited ordered-types)
	    `(setf ,package-use-list (package-%use-list (car ,packages)))
	    `(declare (ignore ,package-use-list)))
       (macrolet ((,init-macro (next-kind)
 	 (let ((symbols (gensym)))
	   `(progn
	      (setf ,',kind ,next-kind)
	      (setf ,',counter nil)
	      ,(case next-kind
		 (:internal
		  `(let ((,symbols (package-internal-symbols
				    (car ,',packages))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))
		 (:external
		  `(let ((,symbols (package-external-symbols
				    (car ,',packages))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))
		 (:inherited
		  `(let ((,symbols (package-external-symbols
				    (car ,',package-use-list))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))))))
		  (,end-test-macro (this-kind)
		     `,(let ((next-kind (cadr (member this-kind
						      ',ordered-types))))
			 (if next-kind
			     `(,',init-macro ,next-kind)
			     `(if (endp (setf ,',packages (cdr ,',packages)))
				  (return-from ,',BLOCK)
				  (,',init-macro ,(car ',ordered-types)))))))
	 (when ,packages
	   ,(when (null symbol-types)
	      (error "Must supply at least one of :internal, :external, or ~
	      :inherited."))
	   ,(dolist (symbol symbol-types)
	      (unless (member symbol '(:internal :external :inherited))
		(error "~S is not one of :internal, :external, or :inherited."
		       symbol)))
	   (,init-macro ,(car ordered-types))
	   (flet ((,real-symbol-p (number)
		    (> number 1)))
	     (macrolet ((,mname ()
	      `(block ,',BLOCK
		 (loop
		   (case ,',kind
		     ,@(when (member :internal ',ordered-types)
			 `((:internal
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :internal)))))
		     ,@(when (member :external ',ordered-types)
			 `((:external
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :external)))))
		     ,@(when (member :inherited ',ordered-types)
			 `((:inherited
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (cond (,',counter
				   (return-from
				    ,',BLOCK
				    (values t (svref ,',vector ,',counter)
					    ,',kind (car ,',packages))))
				  (t
				   (setf ,',package-use-list
					 (cdr ,',package-use-list))
				   (cond ((endp ,',package-use-list)
					  (setf ,',packages (cdr ,',packages))
					  (when (endp ,',packages)
					    (return-from ,',BLOCK))
					  (setf ,',package-use-list
						(package-%use-list
						 (car ,',packages)))
					  (,',init-macro ,(car ',ordered-types)))
					 (t (,',init-macro :inherited)
					    (setf ,',counter nil)))))))))))))
	       ,@body)))))))
