;;; Functions that are more "explicit" versions of Lisp ones.
;;; Generally this means they take only required arguments, there are no designators,
;;; and some arguments are assumed to have been checked for validity already.
;;; This means they do not depend on different global environments and need only be compiled and linked once.

;;; They can be compiled+linked in whatever environment with standard CL, thus flagrant use of macros etc.

(defun explicit:funcall (function &rest arguments)
  (explicit:apply function arguments))
(define-compiler-macro explicit:funcall (function &rest arguments)
  `(cleavir-primop:funcall ,function ,@arguments))

#+clasp
(progn
;;; FIXME, though small priority, an APPLY that doesn't coerce fdesignator
(defun explicit:apply (function &rest spreadable-arguments)
  (cl:apply #'cl:apply function spreadable-arguments))
(define-compiler-macro explicit:apply (function &rest spreadable-arguments)
  `(cl:apply ,function ,@spreadable-arguments))
)

(defun explicit:member (item list test key)
  (loop for ls on list
        when (explicit:funcall test item (explicit:funcall key (car ls)))
          return ls))
(defun explicit:member-not (item list test key)
  (loop for ls on list
        unless (explicit:funcall test item (explicit:funcall key (car ls)))
          return ls))

(defun explicit:member-if (test list key)
  (loop for ls on list
        when (explicit:funcall test (explicit:funcall key (car list)))
          return ls))
(defun explicit:member-if-not (test list key)
  (loop for ls on list
        unless (explicit:funcall test (explicit:funcall key (car list)))
          return ls))

(defun explicit:assoc (item list test key)
  (loop for pair in list
        when (and (not (null pair))
                  (explicit:funcall test item (explicit:funcall key (car pair))))
          return pair))
(defun explicit:assoc-not (item list test key)
  (loop for pair in list
        when (and (not (null pair))
                  (not (explicit:funcall test item (explicit:funcall key (car pair)))))
          return pair))

(defun explicit:assoc-if (test list key)
  (loop for pair in list
        when (and (not (null pair))
                  (explicit:funcall test (explicit:funcall key (car pair))))
          return pair))
(defun explicit:assoc-if-not (test list key)
  (loop for pair in list
        when (and (not (null pair))
                  (not (explicit:funcall test (explicit:funcall key (car pair)))))
          return pair))

;;; From SBCL
;;; Package should be fixed/it should be file local?
(defun explicit::map1 (fun arglists accumulate take-car)
  (do* ((non-acc-result (car arglists))
        (ret-list (list nil))
        (temp ret-list)
        (res nil)
        (args (make-list (length arglists))))
       ((dolist (x arglists) (or x (return t)))
        (if accumulate
            (cdr ret-list)
            non-acc-result))
    (do ((l arglists (cdr l))
         (arg args (cdr arg)))
        ((null l))
      (setf (car arg) (if take-car (caar l) (car l)))
      (setf (car l) (cdar l)))
    (setq res (explicit:apply fun args))
    (case accumulate
      (:nconc
       (when res
         (setf (cdr temp) res)
         ;; KLUDGE: it is said that MAPCON is equivalent to
         ;; (apply #'nconc (maplist ...)) which means (nconc 1) would
         ;; return 1, but (nconc 1 1) should signal an error.
         (when (consp res)
           (setf temp (last res)))))
      (:list (setf (cdr temp) (list res)
                   temp (cdr temp))))))

(macrolet ((defmap (name accumulate take-car)
             `(defun ,name (function list &rest more-lists)
                (explicit::map1 function (cons list more-lists) ,accumulate ,take-car))))
  (defmap explicit:mapc nil t) (defmap explicit:mapl nil nil)
  (defmap explicit:mapcar :list t) (defmap explicit:maplist :list nil)
  (defmap explicit:mapcan :nconc t) (defmap explicit:mapcon :nconc nil))

#+(or)
(progn
#+clasp
(defun explicit:maphash (function table)
  (maphash function table))

;;; FIXME: Signals working environment independently is difficult because it's very
;;; typep based. Typep is used for *break-on-signals* and, more critically, for finding
;;; handlers. But type specifiers are certainly environment dependent.
(defun explicit:signal (condition)
  (signal condition))

(defun explicit:error (condition)
  (explicit:signal condition)
  ;; TODO: infinite error protect?
  ;; Actually, invoke-debugger might be super environment dependent?
  (explicit:invoke-debugger condition))

(defun explicit:warn (condition error-output)
  (restart-case (explicit:signal condition)
    (muffle-warning ()
      :report "Skip warning."
      (return-from explicit:warn nil)))
  (format error-output "~&;;; Warning: ~A~%" condition))
)

#+(or)
(progn
(defgeneric explicit:make-instance (class &rest initargs))
(defmethod explicit:make-instance ((class standard-class) &rest initargs)
  #+clasp
  (progn
    (unless (clos:class-finalized-p class)
      (clos:finalize-inheritance class))
    (setf initargs (clos::add-default-initargs class initargs))
    (let ((keywords (if (slot-boundp class 'clos::valid-initargs)
                        (clos::class-valid-initargs class)
                        (clos::precompute-valid-initarg-keywords class))))
      (clos::check-initargs class initargs nil (clos:class-slots class) keywords)))
  (apply #'initialize-instance (apply #'allocate-instance class initargs) initargs))

(defgeneric explicit:make-load-form (object environment))
(defmethod explicit:make-load-form ((object standard-object) environment)
  (declare (ignore environment))
  (error "Don't know how to dump ~a" object))
(defmethod explicit:make-load-form ((object structure-object) environment)
  (declare (ignore environment))
  (error "Don't know how to dump ~a" object))
(defmethod explicit:make-load-form ((object condition) environment)
  (declare (ignore environment))
  (error "Don't know how to dump ~a" object))

(defmethod explicit:make-load-form ((object class) environment)
  ;; This is a little weird with first class global environments, but the CLHS is explicit:
  ;; If the class has a proper name /in the environment/ we return a find-class form.
  ;; "proper name" is defined in notes of CLHS class-name (and 4.3.1) to be the symbol S such
  ;; that (class-name (find-class S)) = S, in other words, it is attached as the class-name.
  ;; "in [the] environment" obviously means find-class, which we do with sicl-genv.
  (let ((name (class-name object))
        (named (sicl-genv:find-class name environment)))
    ;; If class has no proper name we have to signal an error. FIXME: proper condition types.
    ;; We signal different ones for "class has no name" and "class has no name in environment"
    ;; because those can be pretty different things.
    (cond ((null name)
           (error "Cannot dump anonymous class ~s" object))
          ((eq object named)
           ;; since we're working for the compiler, we can assume/hope that the programmer properly
           ;; makes the compile and load time environments agree; nothing different from standard CL.
           ;; Note that we're allowed to return a form that computes+returns a class in some other way
           ;; if there is no named class in the load environment. For now we don't do this.
           `(find-class ',name))
          (t
           (error "Could not dump ~s: it has no name in environment ~s" object environment)))))
)

;;; I actually am not sure how this could be environment-dependent.
;;; I suppose if it was generic, you could specialize it for environments which... call allocate-instance
;;; something else? Dunno.
#+(or)
(defun explicit:make-load-form-saving-slots (object environment &key (slot-names nil slot-names-p))
  (declare (ignore environment))
  (let ((class (class-of object)))
    (unless slot-names-p
      #+clasp
      (setf slot-names (mapcar #'clos:slot-definition-name (clos:class-slots class))))
    (values
     `(allocate-instance ,class)
     `(progn
        ,@(loop for name in slot-names
                collect `(setf (slot-value ,object ',name) ',(slot-value object name)))))))

#+(or) ; typecase
(defun explicit:macroexpand-1 (form environment hook)
  (typecase form
    (symbol
     (let ((expander (sicl-genv:symbol-macro form environment)))
       (if expander
           (values (explicit:funcall hook expander form environment) t)
           (values form nil))))
    (cons
     (let ((head (first form)))
       (if (symbolp head)
           (let ((expander (sicl-genv:macro-function head environment)))
             (if expander
                 (values (explicit:funcall hook expander form environment) t)
                 (values form nil)))
           (values form nil))))
    (t (values form nil))))

#+(or)
(defun explicit:macroexpand (form environment hook)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expanded)
               (explicit:macroexpand-1 form environment hook)
             (if expanded
                 (setf ever-expanded expanded form expansion)
                 (return (values expansion ever-expanded))))))

#+clasp
(defun explicit:eval (form environment)
  (clasp-cleavir::cclasp-eval form environment))
