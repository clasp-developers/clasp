;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;  Copyright (c) 2015, Daniel Kochmański.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                                setf routines

;;;; A SETF expander is a function. It is called, as seen in GET-SETF-EXPANSION,
;;;; as (funcall expander place env), very similarly to a macro function.

;;; NOTE: At present, source info for setf expanders is simply obtained from the
;;; setf expander function (in source-location.lisp). This has the unintuitive
;;; consequence that for things to work correctly, each DEFSETF or
;;; DEFINE-SETF-EXPANDER or whatever must expand into its own function. This was
;;; not always done.

(in-package "EXT")

(defvar *setf-expanders* (make-hash-table :test #'eq :thread-safe t))

(defun setf-expander (symbol &optional environment)
  (if (core:operator-shadowed-p symbol environment)
      nil
      (values (gethash symbol *setf-expanders*))))
(defun (setf setf-expander) (expander symbol &optional environment)
  (unless (null environment)
    (error "(setf setf-expander) was passed a non-null environment"))
  (setf (gethash symbol *setf-expanders*) expander))

(in-package "SYSTEM")

;; used in setf expansions below
(defun parse-bytespec (bytespec)
  (when (and (consp bytespec)
             (eql (car bytespec) 'byte)
             (consp (cdr bytespec))
             (consp (cddr bytespec))
             (null (cdddr bytespec)))
    (values (cadr bytespec) (caddr bytespec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFSETF
;;;
;;; Actually kind of complicated to implement.
;;; We do create this macro in the builder environment, because the definition
;;; in common-macro-definitions has an environmental contamination problem
;;; due to the nested backquote.

;;; As I understand CLHS 3.4.7, a defsetf lambda list is an ordinary lambda list,
;;; except it can have &environment [name] on the end and no &aux.
;;; This function therefore hackily returns an ordinary lambda list, and
;;; the environment variable if there was one.
;;; FIXME: This is not very error-tolerant. In particular we don't check for &aux.
;;; But fixing it will require a more robust lambda list system.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun extract-defsetf-lambda-list (lambda-list)
  (ext:with-current-source-form (lambda-list)
    (if (or (null lambda-list) (null (rest lambda-list))) ; trivial case
        (values lambda-list nil)
        (let ((last-two (last lambda-list 2)))
          (if (eq (first last-two) '&environment)
              (values (ldiff lambda-list last-two) (second last-two))
              (values lambda-list nil))))))
) ; eval-when

(defmacro defsetf (access-fn &rest rest)
  "Syntax: (defsetf symbol update-fun [doc])
        or
        (defsetf symbol lambda-list (store-var*) {decl | doc}* {form}*)
Defines an expansion
        (setf (SYMBOL arg1 ... argn) value)
        => (UPDATE-FUN arg1 ... argn value)
           or
           (let* ((temp ARG)*)
             (multiple-value-bind (temp-s*)
                 values-form
               rest)
where REST is the value of the last FORM with parameters in
LAMBDA-LIST bound to the symbols TEMP* and with STORE-VAR* bound to
the symbols TEMP-S*.  The doc-string DOC, if supplied, is saved as a
SETF doc and can be retrieved by (documentation 'SYMBOL 'setf)."
  ;; Here's how this works. We return an expander that receives any number
  ;; of arguments and makes gensyms for them, sets them up as the temps
  ;; and such. The only difference between the short and long form is in
  ;; the storing form and how many stores there are, which is set up by
  ;; this multiple-value-bind here.
  (let ((tempsvar (gensym "TEMPS")) (storesvar (gensym "STORES")))
    (multiple-value-bind (store-form-maker env-var doc nstores)
        (if (and (car rest) (symbolp (car rest)))
            ;; Short form. Easy.
            (let ((update-fn (first rest))
                  (documentation (second rest)))
              (values `(append '(,update-fn) ,tempsvar ,storesvar)
                      nil documentation 1))
            ;; Long form. DEFSETF lambda lists are basically ordinary
            ;; lambda lists, so we use an inner function to parse it.
            ;; The inner function has (,@stores ,@provided-lambda-list)
            ;; as its lambda list (after stripping &environment) so it's
            ;; passed (append stores temps).
            (let ((lambda-list (first rest))
                  (stores (second rest))
                  (body (cddr rest)))
              (multiple-value-bind (lambda-list env-var)
                  (extract-defsetf-lambda-list lambda-list)
                (multiple-value-bind (decls body doc)
                    (core:process-declarations body t)
                  (values `(apply (lambda (,@stores ,@lambda-list)
                                    (declare (core:lambda-name ,access-fn) ,@decls)
                                    ,@(when doc (list doc))
                                    (block ,access-fn ,@body))
                                  (append ,storesvar ,tempsvar))
                          env-var doc (length stores))))))
      (let ((real-env-var (or env-var (gensym "ENV")))
            (wholesym (gensym "WHOLE")))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (ext:setf-expander ',access-fn)
                 (lambda (,wholesym ,real-env-var)
                   ,@(when doc (list doc))
                   (declare (core:lambda-name (ext:setf-expander ,access-fn))
                            ,@(unless env-var `((ignore ,real-env-var))))
                   (let ((,tempsvar (mapcar (lambda (f) (declare (ignore f)) (gensym))
                                            (rest ,wholesym)))
                         (,storesvar (list ,@(make-list nstores :initial-element '(gensym "STORE")))))
                     (values ,tempsvar (rest ,wholesym) ,storesvar ,store-form-maker
                             (list* ',access-fn ,tempsvar)))))
           ',access-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operators for general SETF expansions. Protocol explained at top of file.

(defmacro define-setf-expander (access-fn lambda-list &body body
                                &environment env)
  "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*
          {form}*)
Defines the SETF-method for generalized-variables (SYMBOL ...).
When a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs
given in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in
DEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five
values
	(var1 ... vark)
	(form1 ... formk)
	(value-var)
	storing-form
	access-form
in order.  These values are collectively called the five gangs of the
generalized variable (SYMBOL arg1 ... argn).  The whole SETF form is then
expanded into
	(let* ((var1 from1) ... (vark formk)
	       (value-var value-form))
	  storing-form)
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (DOCUMENTATION 'SYMBOL 'SETF)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:setf-expander ',access-fn)
           ,(ext:parse-define-setf-expander access-fn lambda-list body env))
     ',access-fn))

(defun get-setf-expansion (place &optional env)
  "Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values."
  ;; Note that macroexpansion of SETF arguments can only be done via
  ;; MACROEXPAND-1 [ANSI 5.1.2.7].
  ;; This is so that, if a macro form expands into another macro form, and that
  ;; macro has a defined setf expansion, the expansion is preferred over
  ;; expanding the macro.
  (cond ((symbolp place)
         ;; Could be a symbol macro.
         (multiple-value-bind (expansion expanded) (macroexpand-1 place env)
           (if expanded
               ;; It is. Recur.
               (get-setf-expansion expansion env)
               ;; It's not. Simple variable set.
               (let ((store (gensym "STORE")))
                 (values nil nil (list store) `(setq ,place ,store) place)))))
        ((and (consp place) (symbolp (car place)))
         (let ((head (car place)))
           ;; Compound place. First, check for a setf expander.
           (let ((expander (ext:setf-expander head env)))
             (if expander
                 (funcall expander place env)
                 ;; Check for a macro definition.
                 (multiple-value-bind (expansion expandedp)
                     (macroexpand-1 place env)
                   (if expandedp ; expanded, so recur
                       (get-setf-expansion expansion env)
                       ;; Nothing. Default expansion.
                       (let* ((operator (car place))
                              (arguments (cdr place))
                              (temps (mapcar (lambda (f)
                                               (declare (ignore f))
                                               (gensym "TEMP"))
                                             arguments))
                              (store (gensym "STORE")))
                         (values temps arguments (list store)
                                 `(funcall #'(setf ,operator) ,store ,@temps)
                                 `(,operator ,@temps)))))))))
        (t
         (simple-program-error "Invalid syntax: ~s is not a place." place))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Built-in SETF expansions.

(defsetf car (x) (y)
  "Set the first object in a list."
  `(progn (rplaca ,x ,y) ,y))

(defsetf cdr (x) (y)
  "Set the cdr in a list."
  `(progn (rplacd ,x ,y), y))

(defsetf caar (x) (y)
  "Set the car of the first sublist."
  `(progn (rplaca (car ,x) ,y) ,y))

(defsetf cdar (x) (y)
  "Set the cdr of the first sublist."
  `(progn (rplacd (car ,x) ,y) ,y))

(defsetf cadr (x) (y)
  "Set the second object in a list."
  `(progn (rplaca (cdr ,x) ,y) ,y))

(defsetf cddr (x) (y)
  "Set the cdr in the cdr of a list."
  `(progn (rplacd (cdr ,x) ,y) ,y))

(defsetf caaar (x) (y)
  "Set the first object in the caar of a list."
  `(progn (rplaca (caar ,x) ,y) ,y))

(defsetf cdaar (x) (y)
  "Set the cdr of the caar of a list."
  `(progn (rplacd (caar ,x) ,y) ,y))

(defsetf cadar (x) (y)
  "Set the car of the cdar of a list."
  `(progn (rplaca (cdar ,x) ,y) ,y))

(defsetf cddar (x) (y)
  "Set the cdr of the cdar of a list."
  `(progn (rplacd (cdar ,x) ,y) ,y))

(defsetf caadr (x) (y)
  "Set the first object in the cadr of a list."
  `(progn (rplaca (cadr ,x) ,y) ,y))

(defsetf cdadr (x) (y)
  "Set the cdr of the cadr of a list."
  `(progn (rplacd (cadr ,x) ,y) ,y))

(defsetf caddr (x) (y)
  "Set the first object in the cddr of a list."
  `(progn (rplaca (cddr ,x) ,y) ,y))

(defsetf cdddr (x) (y)
  "Set the cdr of the cddr of a list."
  `(progn (rplacd (cddr ,x) ,y) ,y))

(defsetf caaaar (x) (y)
  "Set the car of the caaar of a list."
  `(progn (rplaca (caaar ,x) ,y) ,y))

(defsetf cdaaar (x) (y)
  "Set the cdr of the caaar of a list."
  `(progn (rplacd (caaar ,x) ,y) ,y))

(defsetf cadaar (x) (y)
  "Set the car of the cdaar of a list."
  `(progn (rplaca (cdaar ,x) ,y) ,y))

(defsetf cddaar (x) (y)
  "Set the cdr of the cdaar of a list."
  `(progn (rplacd (cdaar ,x) ,y) ,y))

(defsetf caadar (x) (y)
  "Set the car of the cadar of a list."
  `(progn (rplaca (cadar ,x) ,y) ,y))

(defsetf cdadar (x) (y)
  "Set the cdr of the cadar of a list."
  `(progn (rplacd (cadar ,x) ,y) ,y))

(defsetf caddar (x) (y)
  "Set the car of the cddar of a list."
  `(progn (rplaca (cddar ,x) ,y) ,y))

(defsetf cdddar (x) (y)
  "Set the cdr of the cddar of a list."
  `(progn (rplacd (cddar ,x) ,y) ,y))

(defsetf caaadr (x) (y)
  "Set the car of the caadr of a list."
  `(progn (rplaca (caadr ,x) ,y) ,y))

(defsetf cdaadr (x) (y)
  "Set the cdr of the caadr of a list."
  `(progn (rplacd (caadr ,x) ,y) ,y))

(defsetf cadadr (x) (y)
  "Set the car of the cdadr of a list."
  `(progn (rplaca (cdadr ,x) ,y) ,y))

(defsetf cddadr (x) (y)
  "Set the cdr of the cdadr of a list."
  `(progn (rplacd (cdadr ,x) ,y) ,y))

(defsetf caaddr (x) (y)
  "Set the car of the caddr of a list."
  `(progn (rplaca (caddr ,x) ,y) ,y))

(defsetf cdaddr (x) (y)
  "Set the cdr of the caddr of a list."
  `(progn (rplacd (caddr ,x) ,y) ,y))

(defsetf cadddr (x) (y)
  "Set the car of the cdddr of a list."
  `(progn (rplaca (cdddr ,x) ,y) ,y))

(defsetf cddddr (x) (y)
  "Set the cdr of the cdddr of a list."
  `(progn (rplacd (cdddr ,x) ,y) ,y))

(defsetf first (x) (y)
  "Set the first object in a list."
  `(progn (rplaca ,x ,y) ,y))

(defsetf second (x) (y)
  "Set the second object in a list."
  `(progn (rplaca (cdr ,x) ,y) ,y))

(defsetf third (x) (y)
  "Set the third object in a list."
  `(progn (rplaca (cddr ,x) ,y) ,y))

(defsetf fourth (x) (y)
  "Set the fourth object in a list."
  `(progn (rplaca (cdddr ,x) ,y) ,y))

(defsetf fifth (x) (y)
  "Set the fifth object in a list."
  `(progn (rplaca (cddddr ,x) ,y) ,y))

(defsetf sixth (x) (y)
  "Set the sixth object in a list."
  `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))

(defsetf seventh (x) (y)
  "Set the seventh object in a list."
  `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))

(defsetf eighth (x) (y)
  "Set the eightheighth object in a list."
  `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))

(defsetf ninth (x) (y)
  "Set the ninth object in a list."
  `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))

(defsetf tenth (x) (y)
  "Set the tenth object in a list."
  `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))

(defsetf rest (x) (y)
  "Set the cdr of a list."
  `(progn (rplacd ,x ,y) ,y))

(defsetf bit (array &rest indices) (value) `(setf (aref ,array ,@indices) ,value))
(defsetf sbit (array &rest indices) (value) `(setf (aref ,array ,@indices) ,value))
(defsetf symbol-value set)
(defsetf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))

(define-setf-expander getf (&environment env place indicator
                            &optional (default nil default-p))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let* ((itemp (gensym "itemp")) (store (gensym "store")) (def (gensym "def")))
      (values `(,@vars ,itemp ,@(if default-p (list def) nil))
              `(,@vals ,indicator ,@(if default-p (list default) nil))
              `(,store)
              `(let ((,(car stores) (sys:put-f ,access-form ,store ,itemp)))
                 ,@(if default-p (list def) nil) ; prevent unused variable warning
                 ,store-form
                 ,store)
              `(getf ,access-form ,itemp ,@(if default-p (list def) nil))))))

(defsetf subseq (sequence1 start1 &optional end1)
		(sequence2)
  `(PROGN (REPLACE ,sequence1 ,sequence2 :START1 ,start1 :END1 ,end1)
    ,sequence2))

(define-setf-expander THE (&environment env type place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (values vars vals stores
            (subst `(THE ,type ,(first stores)) (first stores) store-form)
            `(THE ,type ,access-form))))

(define-setf-expander apply (fn &rest rest)
  (unless (and (consp fn)
               (eq (first fn) 'function)
               (symbolp (second fn))
               (null (cddr fn)))
    (simple-program-error "(apply ~s ...) is not a valid place." fn))
  (let ((name (second fn))
        (temps (mapcar (lambda (f) (declare (ignore f)) (gensym "TEMP")) rest))
        (store (gensym "STORE")))
    ;; We don't have (setf bit) or (setf sbit) defined, so we have to do this
    ;; manually. FIXME: Probably just define those functions.
    (when (or (eq name 'bit) (eq name 'sbit)) (setf name 'aref))
    (values temps rest (list store)
            `(apply #'(setf ,name) ,store ,@rest)
            `(apply ,fn ,@rest))))

(define-setf-expander ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (let ((store (gensym "STORE-LDB"))
          (stemp (first stores)))
      (multiple-value-bind (size position) (parse-bytespec bytespec)
        (if size
            ;; We have a (setf (ldb (byte ...) ...) ...) form.
            ;; We can avoid consing the bytespec.
            (let ((sizetemp (gensym "SIZE-TEMP"))
                  (postemp (gensym "POS-TEMP")))
              (values `(,sizetemp ,postemp ,@temps)
                      `(,size ,position ,@vals)
                      `(,store)
                      `(let ((,stemp
                               (%dpb ,store ,sizetemp ,postemp ,access-form)))
                         ,store-form ,store)
                      `(%ldb ,sizetemp ,postemp ,access-form)))
            ;; general case.
            (let ((btemp (gensym "BTEMP")))
              (values `(,btemp ,@temps)
                      `(,bytespec ,@vals)
                      `(,store)
                      `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                         ,store-form ,store)
                      `(ldb ,btemp ,access-form))))))))

(define-setf-expander mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (multiple-value-bind (size position) (parse-bytespec bytespec)
      (let ((store (gensym "STORE-MASK-FIELD"))
            (stemp (first stores)))
        (if size
            ;; literal (byte ...) form- avoid consing
            (let ((sizetemp (gensym "SIZE-TEMP"))
                  (postemp (gensym "POS-TEMP")))
              (values `(,sizetemp ,postemp ,@temps)
                      `(,size ,position ,@vals)
                      `(,store)
                      `(let ((,stemp
                               (%deposit-field ,store ,sizetemp ,postemp ,access-form)))
                         ,store-form ,store)
                      `(%mask-field ,sizetemp ,postemp ,access-form)))
            ;; general case
            (let ((btemp (gensym "BTEMP-MASK-FIELD")))
              (values `(,btemp ,@temps)
                      `(,bytespec ,@vals)
                      `(,store)
                      `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                         ,store-form ,store)
                      `(mask-field ,btemp ,access-form))))))))

(define-setf-expander values (&rest values &environment env)
  (let ((all-vars '())
	(all-vals '())
	(all-stores '())
	(all-storing-forms '())
	(all-get-forms '()))
    (dolist (item (reverse values))
      (multiple-value-bind (vars vals stores storing-form get-form)
	  (get-setf-expansion item env)
	;; If a place has more than one store variable, the other ones
	;; are set to nil.
	(let ((extra (rest stores)))
	  (unless (endp extra)
	    (setq vars (append extra vars)
		  vals (append (make-list (length extra)) vals)
		  stores (list (first stores)))))
	(setq all-vars (append vars all-vars)
	      all-vals (append vals all-vals)
	      all-stores (append stores all-stores)
	      all-storing-forms (cons storing-form all-storing-forms)
	      all-get-forms (cons get-form all-get-forms))))
    (values all-vars all-vals all-stores `(values ,@all-storing-forms)
	    `(values ,@all-get-forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SETF itself, and related standard macros.

(defun trivial-setf-form (place vars stores store-form access-form)
  (declare (optimize (speed 3) (safety 0)))
  (and (atom place)
       (null vars)
       (eq access-form place)
       (= (length stores) 1)
       (listp store-form)
       (= (length store-form) 3)
       (member (first store-form) '(setq setf))
       (eq (second store-form) place)
       (eq (third store-form) (first stores))
       ))

(defun try-simpler-expansion (place vars vals stores newvalue store-form)
  ;; When the store form contains all the original arguments in order
  ;; followed by a single stored value, we can produce an expansion
  ;; without LET forms.
  (declare (optimize (speed 3) (safety 0)))
  (when (and (consp place)
	     (consp store-form)
	     (= (length place) (the fixnum (1- (length store-form)))))
    (let ((function (pop store-form))
	  (output '())
	  v)
      (dolist (i (rest place)
	       (when (eq (first stores) (first store-form))
		 (list* function
			(nreverse (cons newvalue output)))))
	(unless (consp store-form)
	  (return nil))
	(setq v (car (the cons store-form))
	      store-form (cdr (the cons store-form)))
	;; This checks that the argument at this position coincides with
	;; the corresponding value in the original list. Note that the
	;; variable list need not be in order.
	(unless (or (eq v i)
		    (and (eq v (pop vars))
			 (eq (pop vals) i)))
	  (return nil))
	(push i output)))))

;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue env)
  (declare (notinline mapcar))
  (ext:with-current-source-form (place)
    (multiple-value-bind (vars vals stores store-form access-form)
        (get-setf-expansion place env)
      (cond ((trivial-setf-form place vars stores store-form access-form)
             (list 'setq place newvalue))
            ((try-simpler-expansion place vars vals stores newvalue store-form))
            (t
             `(let* ,(mapcar #'list vars vals)
                (multiple-value-bind ,stores ,newvalue
                  ,store-form)))))))

(defun setf-expand (l env)
  (cond ((endp l) nil)
        ((endp (cdr l)) (simple-program-error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l) env)
               (setf-expand (cddr l) env)))))

;;; SETF macro.
(defmacro setf (&environment env &rest rest)
  "Syntax: (setf {place form}*)
Evaluates each FORM and assigns the value to the corresponding PLACE in order.
Returns the value of the last FORM.
Each PLACE may be any one of the following:
  * A symbol that names a variable.
  * A function call form whose first element is the name of the following
    functions:
	nth	elt	subseq	rest	first ... tenth
	c?r	c??r	c???r	c????r
	aref	svref	char	schar	bit	sbit	fill-pointer
	get	getf	documentation	symbol-value	symbol-function
	symbol-plist	macro-function	gethash		fdefinition
	char-bit	ldb	mask-field
	apply	slot-value
    where '?' stands for either 'a' or 'd'.
  * A function call form whose first element is:
        1. an access function for a structure slot
        2. an accessor method for a CLOS object
  * the form (THE type place) with PLACE being a place recognized by SETF.
  * a macro call which expands to a place recognized by SETF.
  * any form for which a DEFSETF or DEFINE-SETF-EXPANDER declaration has been
    made."
  (cond ((endp rest) nil)
        ((endp (cdr rest))
         (simple-program-error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
        (t (cons 'progn (setf-expand rest env)))))

;;; PSETF and PSETQ.
;;; The latter redefines an early definition in evalmacros.lisp that
;;; doesn't handle symbol macro places correctly.

(labels (;; we pushed these things left to right,
         ;; so we have to reverse them to get the
         ;; proper left-to-right evaluation order of subforms.
         (build (temp-groups value-groups store-forms)
           ;; temp-groups and value-groups have the same length by construction.
           (if (null temp-groups)
               `(progn ,@store-forms nil)
               (let ((temp-bindings (car temp-groups))
                     (next-temp-groups (cdr temp-groups))
                     (stores (caar value-groups))
                     (subform (cdar value-groups))
                     (next-value-groups (cdr value-groups)))
                 `(let* ,temp-bindings
                    (multiple-value-bind ,stores ,subform
                      ,(build next-temp-groups next-value-groups store-forms))))))
         (expand (rest env operator single)
           (when (and (consp rest) (consp (cdr rest)) (null (cddr rest)))
             (return-from expand `(progn (,single ,@rest) nil)))
           (do ((r rest (cddr r))
                ;; a list of lists of let* bindings.
                (temp-groups nil)
                ;; a list of (list-of-store-variables . subform)
                (value-groups nil)
                (store-forms nil))
               ((endp r)
                (build (nreverse temp-groups) (nreverse value-groups)
                       store-forms))
             (when (endp (cdr r))
               (simple-program-error "~S is an illegal ~s form"
                                     `(,operator ,@rest) operator))
             (let ((place (car r)) (subform (cadr r)))
               (ext:with-current-source-form (place)
                 (when (and (eq operator 'psetq)
                            (not (symbolp place)))
                   (simple-program-error "~s is an illegal ~s form"
                                         `(,operator ,@rest) operator))
                 (multiple-value-bind
                       (temps values stores store-form access-form)
                     (get-setf-expansion place env)
                   (declare (ignore access-form))
                   ;; FIXME?: We should maybe signal an error if temps and values
                   ;; have different lengths (i.e. setf expander is broken)
                   (setq temp-groups (cons (mapcar #'list temps values)
                                           temp-groups))
                   (setq value-groups (cons (cons stores subform) value-groups))
                   (setq store-forms (cons store-form store-forms))))))))
  (defmacro psetf (&environment env &rest rest)
    "Syntax: (psetf {place form}*)
Similar to SETF, but evaluates all FORMs first, and then assigns each value to
the corresponding PLACE.  Returns NIL."
    (expand rest env 'psetf 'setf))
  (defmacro psetq (&environment env &rest args)
    "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
    (expand args env 'psetq 'setq)))

;;; Defined in build because common-macro-definitions lacks it.
;;; DEFINE-MODIFY-MACRO macro, by Bruno Haible.
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  "Syntax: (define-modify-macro symbol lambda-list function-name [doc])
Defines a read-modify-write macro like INCF.  The defined macro will expand
a form (SYMBOL place form1 ... formn) into a form that in effect SETFs the
value of (FUNCTION-NAME place arg1 ... argm) into PLACE, where ARG1 ... ARGm
are parameters in LAMBDA-LIST which are bound to FORM1 ... FORMn.  For
example, INCF could be defined as
	(define-modify-macro incf (&optional (x 1)) +)
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (DOCUMENTATION 'SYMBOL 'FUNCTION)."
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
                 (setq restvar (second lambdalistr))
                 (simple-program-error "In the definition of ~S: &REST variable ~S should be a symbol."
                                       name (second lambdalistr)))
             (if (null (cddr lambdalistr))
                 (return)
                 (simple-program-error "Only one variable is allowed after &REST, not ~S"
                                       lambdalistr)))
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (simple-program-error "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S"
                                   next))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist))
            (t (simple-program-error "lambda list may only contain symbols and lists, not ~S"
                                     next))))
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (&ENVIRONMENT ENV %REFERENCE ,@lambdalist)
       ,@(and docstring (list docstring))
       (DECLARE (NOTINLINE MAPCAR))
       (MULTIPLE-VALUE-BIND (VARS VALS STORES SETTER GETTER)
           (GET-SETF-EXPANSION %REFERENCE ENV)
         (LET ((ALL-VARS (MAPCAR #'(LAMBDA (V) (LIST (GENSYM) V)) (LIST* ,@varlist ,restvar))))
           (IF (SYMBOLP GETTER)
               (SUBST (LIST* (QUOTE ,function) GETTER (MAPCAR #'CAR ALL-VARS))
                      (CAR STORES)
                      `(LET* ,ALL-VARS
                         ,SETTER))
               (DO ((D VARS (CDR D))
                    (V VALS (CDR V))
                    (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
                   ((NULL D)
                    (SETQ LET-LIST
                          (LIST*
                           (LIST
                            (CAR STORES)
                            (LIST* (QUOTE ,function) GETTER (MAPCAR #'CAR ALL-VARS)))
                           (APPEND ALL-VARS LET-LIST)))
                    `(LET* ,(NREVERSE LET-LIST)
                       ,SETTER)))))))))

;;; Some macro definitions.

(defmacro remf (&environment env place indicator)
  "Syntax: (remf place form)
Removes the property specified by FORM from the property list stored in PLACE.
Returns T if the property list had the specified property; NIL otherwise."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((s (gensym "s")))
      `(let* (,@(mapcar #'list vars vals) (,s ,indicator))
         (multiple-value-bind (,(car stores) flag)
             (sys:rem-f ,access-form ,s)
           ,store-form
           flag)))))

(define-modify-macro incf (&optional (delta 1)) +
  "Syntax: (incf place [form])
Increments the value of PLACE by the value of FORM.  FORM defaults to 1.")

(define-modify-macro decf (&optional (delta 1)) -
  "Syntax: (decf place [form])
Decrements the value of PLACE by the value of FORM.  FORM defaults to 1.")

(defmacro push (&environment env item place)
  "Syntax: (push form place)
Evaluates FORM, conses the value of FORM to the value stored in PLACE, and
makes it the new value of PLACE.  Returns the new value of PLACE."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (when (trivial-setf-form place vars stores store-form access-form)
      (return-from push `(setq ,place (cons ,item ,place))))
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item env)
      (setq vals (cons item vals)
	    item (gensym "pushval")
	    vars (cons item vars)))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list (list 'cons item access-form))))
       ,store-form)))

(defmacro pushnew (&environment env item place
                   &rest rest &key test test-not key)
  "Syntax: (pushnew form place {keyword-form value-form}*)
Evaluates FORM first.  If the value is already in the list stored in PLACE,
does nothing.  Else, conses the value onto the list and makes the result the
new value of PLACE.  Returns NIL.  KEYWORD-FORMs and VALUE-FORMs are used to
check if the value of FORM is already in PLACE as if their values are passed
to MEMBER."
  (declare (notinline mapcar))
  (declare (ignore test test-not key))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (when (trivial-setf-form place vars stores store-form access-form)
      (return-from pushnew `(setq ,place (adjoin ,item ,place ,@rest))))
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item env)
      (setq vals (cons item vals)
	    item (gensym "pushnew")
	    vars (cons item vars)))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals
			    (list (list* 'adjoin item access-form rest))))
       ,store-form)))


(defmacro pop (&environment env place)
  "Syntax: (pop place)
Gets the cdr of the value stored in PLACE and makes it the new value of PLACE.
Returns the car of the old value in PLACE."
  (declare (notinline mapcar))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((store-var (first stores)))
      `(let* ,(mapcar #'list
                      (append vars stores)
                      (append vals (list access-form)))
         (prog1 (car ,store-var)
           (setq ,store-var (cdr (the list ,store-var)))
           ,store-form)))))

(defmacro multiple-value-setq ((&rest vars) form)
  "Syntax: (multiple-value-setq {var}* form)

Evaluates FORM and binds the N-th VAR to the N-th value of FORM or, if FORM
returns less than N values, to NIL.  Returns the first value of FORM or, if
FORM returns no value, NIL."
  (if (null vars)
      `(values ,form)
      `(values (setf (values ,@vars) ,form))))
