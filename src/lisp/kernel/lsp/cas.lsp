(in-package "MP")

(defun cas-expander (symbol)
  (core:get-sysprop symbol 'cas-method))
(defun (setf cas-expander) (expander symbol)
  (core:put-sysprop symbol 'cas-method expander))

(defmacro cas (place old new &environment env)
  "(CAS place old new)
Atomically store NEW in PLACE if OLD matches the current value of PLACE.
Matching is as if by EQ.
Returns the previous value of PLACE; if it's EQ to OLD the swap happened.
Only the swap is atomic. Evaluation of PLACE's subforms, OLD, and NEW is
not guaranteed to be in any sense atomic with the swap, and likely won't be.
PLACE must be a CAS-able place. CAS-able places are either symbol macros,
or accessor forms with a CAR of
SYMBOL-VALUE,
or a macro,
or an accessor defined with DEFINE-CAS-EXPANDER.
Some CAS accessors have additional semantic constraints.
You can see their documentation with e.g. (documentation 'slot-value 'mp:cas)
This is planned to be expanded to include SVREF, SYMBOL-VALUE, variables,
possibly other simple vectors, and slot accessors.
Experimental."
  (multiple-value-bind (temps values oldvar newvar cas read)
      (get-cas-expansion place env)
    (declare (ignore read))
    `(let* (,@(mapcar #'list temps values)
            (,oldvar ,old) (,newvar ,new))
       ,cas)))

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,old ,read))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas))
             finally (return ,new)))))

(defmacro atomic-incf (place &optional (delta 1))
  `(atomic-update ,place #'+ ,delta))

(defmacro atomic-decf (place &optional (delta 1))
  `(atomic-update ,place #'(lambda (y x) (- x y)) ,delta))

(defun get-cas-expansion (place &optional env)
  "Analogous to GET-SETF-EXPANSION. Returns the following six values:
* list of temporary variables, which will be bound as if by LET*
* list of forms, whose results will be bound to the variables
* variable for the old value of PLACE
* variable for the new value of PLACE
* A form to perform the swap, which can refer to the temporary variables
   and the variables for the old and new values
* A form to read a value from PLACE, which can refer to the temporary variables"
  (etypecase place
    (symbol
     (multiple-value-bind (expansion expanded)
         (macroexpand-1 place env)
       (if expanded
           (get-cas-expansion expansion env)
           (error "CAS on variables not supported yet")))
     #+(or)
     (let ((info (cleavir-env:variable-info env place)))
       (etypecase info
         (cleavir-env:symbol-macro-info
          (get-cas-expansion (macroexpand-1 place env) env))
         (cleavir-env:special-variable-info
          (get-cas-expansion `(symbol-value ',place) env))
         (cleavir-env:lexical-variable-info
          (lexical-cas-expansion place env)))))
    (cons
     (let* ((name (car place))
            (expander (cas-expander name)))
       (if expander
           (funcall expander place env)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 place env)
             (if expanded
                 (get-cas-expansion expansion env)
                 (default-cas-expansion place env))))))))

#+(or)
(defun lexical-cas-expansion (var &optional env)
  ;; So: For a regular local, cas is meaningless.
  ;; We can reasonably say it succeeds, i.e.
  ;; (cas x old new) = (prog1 old (setq x new))
  ;; For a closed over variable, we could do an
  ;; actual CAS. Closures are just objects, so
  ;; I think this is even reasonable. But to
  ;; support it we kind of need a special form
  ;; so that the compiler can determine the
  ;; closed-over-ness of the variable.
  ;; ...but none of this is supported right now.
  (let ((old (gensym "OLD")) (new (gensym "NEW")))
    (values nil nil old new
            `(casq ,var ,old ,new)
            var)))

(defun default-cas-expansion (place &optional env)
  (declare (ignore env))
  (error "~a is not a supported place to CAS" place)
  #+(or)
  (let* ((op (car place)) (args (cdr place))
         (temps (loop for form in args collect (gensym)))
         (new (gensym "NEW")) (old (gensym "OLD")))
    (values temps args old new
            `(funcall #'(cas ,op) ,@temps)
            `(,op ,@temps))))

(defmacro define-cas-expander (name lambda-list &body body
                               &environment env)
  "Analogous to DEFINE-SETF-EXPANDER, defines a CAS expander for ACCESSOR.
The body must return the six values for GET-CAS-EXPANSION.
It is up to you the definer to ensure the swap is performed atomically.
This means you will almost certainly need Clasp's synchronization operators
(e.g., CAS on some other place).

Docstrings are accessible with doc-type MP:CAS."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (cas-expander ',name)
           ,(ext:parse-macro name lambda-list body env))
     ',name))

;;; Internal, but kind of like DEFSETF.
(defmacro define-simple-cas-expander (name cas-op (&rest params))
  (let ((scmp (gensym "CMP")) (snew (gensym "NEW"))
        (stemps (loop repeat (length params) collect (gensym))))
    `(define-cas-expander ,name (,@params)
       (let ((,scmp (gensym "CMP")) (,snew (gensym "NEW"))
             ,@(loop for st in stemps
                     collect `(,st (gensym "TEMP"))))
         (values (list ,@stemps) (list ,@params) ,scmp ,snew
                 (list ',cas-op ,scmp ,snew ,@stemps)
                 (list ',name ,@stemps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation support
;;;

(defmethod documentation ((object symbol) (doc-type (eql 'cas)))
  (let ((exp (cas-expander object)))
    (when exp (documentation exp t))))

(defmethod (setf documentation) (new (object symbol) (doc-type (eql 'cas)))
  (let ((exp (cas-expander object)))
    (if exp
        (setf (documentation exp t) new)
        new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Particular CAS expansions
;;;

(define-cas-expander the (type place &environment env)
  "(cas (the y x) o n) = (cas x (the y o) (the y n))"
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    (values vars vals old new
            `(let ((,old (the ,type ,old))
                   (,new (the ,type ,new)))
               ,cas)
            `(the ,type ,read))))

(define-simple-cas-expander symbol-value core:cas-symbol-value (symbol))
(define-simple-cas-expander symbol-plist core:cas-symbol-plist (symbol))
