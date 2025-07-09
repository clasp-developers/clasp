(in-package #:clos)

(defgeneric compute-effective-method (generic-function method-combination
                                      applicable-methods))

(defmethod compute-effective-method ((gf standard-generic-function)
                                     method-combination applicable-methods)
  (let ((compiler (method-combination-compiler method-combination))
        (options (method-combination-options method-combination)))
    (apply compiler gf applicable-methods options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EFFECTIVE METHOD FUNCTIONS
;;;
;;; Effective method functions are the functional version of effective
;;; methods (effective methods being the forms returned by
;;; compute-effective-method). On Clasp, they are functions that accept the
;;; same arguments as the generic function.
;;; In general we can simply compile the effective method, but the compiler
;;; is slow, so we go to some effort to special case common effective
;;; methods.
;;; Note that we more often go through this mechanism than putting the
;;; effective methods in the discriminating function directly. See
;;; *inline-effective-methods* in discriminate.lisp.
;;; The main entry to this section is EFFECTIVE-METHOD-FUNCTION, which
;;; returns a function for a given effective method.
;;; The ARG-INFO threaded throughout here is used to skip some APPLYing.
;;; See miss.lisp, gf-arg-info function.

;;; We pass the parameters to CALL-METHOD and sundry in this fashion.
(defmacro with-effective-method-parameters ((&rest spreadable) &body body)
  `(symbol-macrolet ((+emf-params+ (,@spreadable))) ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun effective-method-parameters (&optional environment)
    (multiple-value-bind (expansion expanded)
        (macroexpand-1 '+emf-params+ environment)
      (if expanded
          expansion
          ;; If we're not in a discriminator, and so the symbol macro
          ;; isn't bound, we return a banal response.
          ;; FIXME?: Might want to signal an error instead.
          ;; .method-args. isn't as universal any more.
          (values nil '.method-args.)))))

(defvar *avoid-compiling* nil)

(defun emf-maybe-compile (form)
  (if *avoid-compiling*
      (let ((cmp:*cleavir-compile-hook* nil))
        (declare (special cmp:*cleavir-compile-hook*))
        (compile nil form))
      (let ((*avoid-compiling* t))
        (compile nil form))))

(defun emf-default (form arg-info)
  (let ((rest (first (last arg-info))) (vars (butlast arg-info)))
    (emf-maybe-compile
     `(lambda (,@vars ,@(when rest `(&rest ,rest)))
        (with-effective-method-parameters (,@arg-info)
          ,form)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-method-form-p (form)
    (and (consp form)
      (eq (first form) 'make-method)
      (consp (cdr form))
      (null (cddr form)))))

(defun emf-from-contf (contf method next-methods arg-info)
  (let ((next (if (null next-methods)
                  (make-%no-next-method-continuation method)
                  (emf-call-method
                   (first next-methods) (list (rest next-methods))
                   arg-info))))
    (lambda (&rest .method-args.)
      (apply contf next .method-args.))))

(defun emf-call-method (method rest arg-info)
  (cond ((make-method-form-p method)
         ;; FIXME: Should call-next-method etc be bound
         (effective-method-function (second method) arg-info))
        ((eq (class-of (method-function method))
             (load-time-value (find-class '%leaf-method-function)))
         ;; leaf method functions are valid EMFs.
         (fmf (method-function method)))
        ((eq (class-of (method-function method))
             (load-time-value (find-class '%contf-method-function)))
         (destructuring-bind (&optional next-methods) rest
           (emf-from-contf
            (contf (method-function method))
            method next-methods arg-info)))
        ;; Could be a nonstandard method with its own EXPAND-APPLY-METHOD.
        (t (emf-default `(call-method ,method ,@rest) arg-info))))

(defun effective-method-function (form &optional (arg-info '(emf-more)))
  ;; emf-default is always valid, but let's pick off a few cases
  ;; so that we can avoid using the compiler, which is slow.
  (if (consp form)
      (case (first form)
        ;; Note that MAKE-METHOD is not valid outside of a CALL-METHOD,
        ;; so form shouldn't be a MAKE-METHOD form.
        ((call-method) (emf-call-method (second form) (cddr form) arg-info))
        (otherwise (emf-default form arg-info)))
      (emf-default form arg-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-METHOD
;;;

(defun argforms-to-arg-info (argforms &optional env)
  (let* ((final (first (rest argforms)))
         (butl (butlast argforms)))
    (cons (and (constantp final env)
               (null (ext:constant-form-value final env)))
          (loop for s in butl
                if (symbolp s) collect (make-symbol (symbol-name s))
                  else collect (gensym "REQ-ARG")))))

(defgeneric methodp (object))
(defmethod methodp ((object method)) t)
(defmethod methodp (object) (declare (ignore object)) nil)

;;; Convert an element of the second argument of a usual call-method
;;; into a method or form producing a method.
(defun call-method-aux (gf method &optional (arg-info '(emf-more)))
  (cond ((methodp method) method)
        ((make-method-form-p method)
         `(make-instance ,(generic-function-method-class gf)
            ;; FIXME?: These are of course lies.
            ;; Our own method on shared-initialize will signal an error
            ;; without these initargs, though.
            :specializers '()
            :qualifiers '()
            :lambda-list '()
            ;; FIXME: Should call-next-method etc be available?
            :function (make-%leaf-method-function
                       (effective-method-function
                        ',(second method) ',arg-info))))
        ;; FIXME: Delay this? Right now this error occurs during
        ;; macroexpansion of CALL- or APPLY-METHOD.
        (t (error "Invalid argument to CALL-METHOD: ~a" method))))

;;; Convert the second argument of a usual call-method into a list
;;; of methods.
(defun call-method-next-methods (gf next-methods &optional (arg-info '(emf-more)))
  (declare (ignore arg-info))
  (loop for nmethod in next-methods
        collect (call-method-aux gf nmethod)))

(defgeneric expand-apply-method (method method-arguments arguments env))

(defmethod expand-apply-method ((method effective-reader-method)
                                method-arguments arguments env)
  (declare (ignore method-arguments))
  (let* ((location (effective-accessor-method-location method))
         (sname (slot-definition-name
                 (accessor-method-slot-definition method)))
         (valuef
           (cond ((core:fixnump location)
                  ;; instance location- easy
                  `(standard-instance-access ,(first arguments) ',location))
                 ((consp location)
                  ;; class location. we need to find the new cell at load time.
                  `(car ,(class-cell-form sname
                                          (first (method-specializers method)))))
                 (t
                  (error "BUG: Slot location ~a is not a fixnum or cons" location)))))
    `(let ((value ,valuef))
       (if (core:sl-boundp value)
           value
           (slot-unbound (class-of ,(first arguments))
                         ,(first arguments)
                         ',sname)))))

(defmethod expand-apply-method ((method effective-writer-method)
                                method-arguments arguments env)
  (declare (ignore method-arguments))
  (let ((location (effective-accessor-method-location method))
        (sname (slot-definition-name
                (accessor-method-slot-definition method)))
        (class (second (method-specializers method))))
    (cond ((core:fixnump location)
           `(setf (standard-instance-access ,(second arguments) ,location)
                  ,(first arguments)))
          ((consp location)
           ;; class location
           ;; Note we don't actually need the instance.
           `(setf (car ,(class-cell-form sname class)) ,(first arguments)))
          (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))

(defun class-cell-form (slot-name class)
  `(load-time-value
    (slot-definition-location
     (or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
         (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                ',slot-name ,class)))))

(defmethod expand-apply-method ((method standard-method)
                                method-arguments arguments env)
  ;; should be (&optional ((&rest next-methods))) but ecclesia is stupid
  (destructuring-bind (&optional next-methods) method-arguments
    (let ((arg-info (argforms-to-arg-info arguments env)))
      (cond
        ;; TODO: General inlining mechanism might be good.
        ((eq (class-of (method-function method))
             (load-time-value (find-class '%leaf-method-function)))
         `(apply
           (load-time-value (fmf (method-function ,method)) t)
           ,@arguments))
        ((eq (class-of (method-function method))
             (load-time-value (find-class '%contf-method-function)))
         `(apply
           (load-time-value (contf (method-function ,method)) t)
           (load-time-value
            ,(if (null next-methods)
                 `(make-%no-next-method-continuation
                   ,method)
                 `(emf-call-method
                   ',(first next-methods)
                   '(,(rest next-methods)) ',arg-info))
            t)
           ,@arguments))
        ;; Default: AMOP protocol.
        (t `(funcall (load-time-value (method-function ,method) t)
                     ;; last element might be a vaslist
                     (apply #'list ,@arguments)
                     (load-time-value
                      (list ,@(call-method-next-methods
                               (method-generic-function method)
                               next-methods arg-info))
                      t)))))))

(defmacro apply-method (method (&rest method-arguments) &rest arguments
                        &environment env)
  "Call the given method. METHOD-ARGUMENTS are the unevaluated arguments
passed in a CALL-METHOD form after the method.
ARGUMENTS is a list of forms that will evaluate to a spreadable
argument list designator."
  (expand-apply-method method method-arguments arguments env))

(let ()
  ;; This macro is only needed in the running system when generic functions miss,
  ;; so we make sure it's not top level.
  ;; Maybe should just be defined later?
  (defmacro call-method (method &rest method-arguments &environment env)
    (if (make-method-form-p method)
        (second method) ; FIXME: should we try to bind CALL-NEXT-METHOD etc?
        `(apply-method ,method (,@method-arguments)
                       ,@(effective-method-parameters env)))))
