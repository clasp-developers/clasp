(defmacro defun (name lambda-list &body body)
  (multiple-value-bind (decls body docstring)
      (core:process-declarations body t)
    `(progn
       (eval-when (:compile-toplevel)
         (declare-function ',name ',lambda-list))
       (eval-when (:load-toplevel :execute)
         (setf (fdefinition ',name)
               (lambda ,lambda-list
                 ,@(when docstring (list docstring))
                 (declare (core:lambda-name ,name)
                          ,@decls)
                 (block ,(core:function-block-name name)
                   ,@body))))
       ',name)))

#+(or)
(defmacro define-symbol-macro (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:symbol-macro ',symbol) ',expansion)
     ',symbol))

#+(or)
(defmacro defparameter (name initial-value &optional documentation)
  (declare (ignorable documentation))
  `(progn
     (declaim (special ,name))
     (setf (symbol-value ',name) ,initial-value)
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

#+(or)
(defmacro defvar (name &optional (initial-value nil ivp) documentation)
  (declare (ignorable documentation))
  `(progn
     (declaim (special ,name))
     ,@(when ivp
         `((unless (boundp ',name)
             (setf (symbol-value ',name) ,initial-value))))
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

#+(or)
(defmacro define-compiler-macro (name lambda-list &body body)
  (multiple-value-bind (function pprint docstring)
      (core::expand-defmacro name lambda-list body 'define-compiler-macro)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name)
             #',function)
       #+documentation
       ,@(when docstring
           `((setf (documentation ',name 'compiler-macro) ,docstring)))
       ',name)))

#+(or)
(defmacro defconstant (name value &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (ext:constant-variable ',name) ,value)
     #+documentation
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

#+(or)
(defmacro defgeneric (function-name gf-lambda-list &rest options-and-methods)
  (loop for (key . rest) in options-and-methods
        if (eq key 'declare)
          append rest into declarations
        if (eq key ':declare) ; as specified by ensure-generic-function
          append rest into declarations
        if (eq key ':declarations) ; as specified by ensure-generic-function-using-class
          append rest into declarations
        if (eq key ':method)
          append rest into methods
        if (eq key ':method-combination)
          append (list :method-combination
                       ;; CLHS says ensure-generic-function gets a MC /object/, not just a spec
                       ;; this works better for us anyway, the less functions that need an environment
                       ;; internally the better (though e-g-f takes :environment also....)
                       ;; However the MOP function get a method combination takes a gf as its first
                       ;; argument. I don't know why- what's it going to do with it? Clasp/ECL and SBCL
                       ;; both ignore it.
                       `(clos:find-method-combination nil ',(first rest) ',(rest rest)))
            into initargs
        else append (cons key (mapcar (lambda (thing) `',thing) rest)) into initargs
        finally
           (return
             `(progn
                (ensure-generic-function
                 ',function-name
                 :lambda-list ',gf-lambda-list
                 :environment (load-time-value (sicl-genv:global-environment))
                 ,@(when declarations
                     `(:declarations ,@declarations))
                 ,@initargs)
                ,@(when methods
                    `((clos::associate-methods-to-gfun
                       (fdefinition ',function-name)
                       ,@(loop for method in methods collecting `(defmethod ,@method)))))))))

#+(or)
(defmacro defmethod (function-name &rest args &environment env)
  (let* ((ll-pos (position-if #'listp args))
         (qualifiers (subseq args 0 ll-pos))
         (args (subseq args ll-pos)))
    (destructuring-bind (lambda-list &rest body-decls) args
      (let ((unspecd (clos:extract-lambda-list lambda-list))
            (specializers (clos:extract-specializer-names lambda-list))
            ;; CLHS DEFMETHOD says the gf lambda list is congruent,
            ;; and has &key but not key parameters if the method lambda list does.
            (gf-ll (loop for x in lambda-list
                         with clear-state = nil
                         when (eq x '&key)
                           collect x
                           and do (setf clear-state t)
                         when clear-state
                           when (eq x '&allow-other-keys) collect x end
                             else collect x)))
        (let ((gf (when (fboundp function-name) (fdefinition function-name))))
          (if gf
              nil ; (check-type gf generic-function)
              (setf gf (ensure-generic-function function-name :lambda-list gf-ll)))
          (multiple-value-bind (lambda-form declarations documentation)
              (clos::make-raw-lambda function-name unspecd (subseq unspecd 0 (length specializers))
                                     specializers body-decls env qualifiers)
            (let ((method-class (clos:generic-function-method-class gf)))
              (multiple-value-bind (lambda options)
                  (clos:make-method-lambda gf (clos:class-prototype method-class) lambda-form env)
                `(let ((method (make-instance ,method-class ; let make-load-form handle it
                                 :function ,lambda
                                 :specializers (list ,@(loop for spec in specializers
                                                             when (symbolp spec)
                                                               collect `(find-class ',spec)
                                                             else
                                                               collect `(clos:intern-eql-specializer ,(second spec))))
                                 :qualifiers ',qualifiers
                                 :lambda-list ,lambda-list
                                 ,@options)))
                   (add-method (ensure-generic-function ',function-name :lambda-list ',gf-ll)
                               method))))))))))
