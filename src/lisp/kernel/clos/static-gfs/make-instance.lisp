(in-package #:static-gfs)

(defun make-instance-form (class keys params)
  (let ((patch-list
          (list
           (cons (find-method #'make-instance nil (list (find-class 'class)))
                 #'standard-make-instance-form)))
        (methods (compute-applicable-methods #'make-instance (list class))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'make-instance methods (list class keys params) patch-list
         (reconstruct-arguments keys params))
        (default-make-instance-form class keys params))))

(defun default-make-instance-form (class keys params)
  ;; This is strictly worse than not inlining,
  ;; but it's rare to define methods on make-instance.
  `(locally
       (declare (notinline make-instance))
     (make-instance (find-class ',(class-name class)
                                ,@(loop for key in keys for param in params
                                        collect `',key collect param)))))

(defun initarg-error-form (class bad-initargs)
  ;; TODO: proper error
  `(error "Unknown initialization arguments for ~s: ~s"
          ',class ',bad-initargs))

(defun standard-make-instance-form (class keys params)
  (multiple-value-bind (keys params bindings)
      (default-initargs class keys params)
    (let ((bad-initargs (bad-initargs class keys))
          (instance (gensym "INSTANCE")))
      `(let (,@bindings)
         (progn
           ,@(unless (null bad-initargs)
               ;; We have unknown initargs,
               ;; but maybe :allow-other-keys was supplied.
               (let ((pos (position :allow-other-keys keys :test #'eq)))
                 (if pos
                     (let ((aok-param (nth pos params)))
                       `((unless ,aok-param
                           ,(initarg-error-form class bad-initargs))))
                     (return-from standard-make-instance-form
                       (initarg-error-form class bad-initargs)))))
           ;; See allocate-instance.lisp
           (let ((,instance (static-allocate-instance
                             ,class
                             ,@(loop for key in keys for param in params
                                     collect `',key collect param))))
             ,(initialize-instance-form class instance keys params)))))))

;; Returns a defaulted list of keys and list of params, as well as a list
;; of bindings for any new params to the default values.
(defun default-initargs (class keys params)
  (let ((defaults (remove-if (lambda (initarg) (member initarg keys :test #'eq))
                             (clos:class-default-initargs class)
                             ;; MOP says a canonical initarg spec is
                             ;; (key form function)
                             :key #'first)))
    (loop for (key form function) in defaults
          for bind = (make-symbol (symbol-name key))
          ;; Verbose for COMPILE-FILE reasons
          for function-form
            = `(load-time-value
                (let ((spec (find ',key (clos:class-default-initargs ,class)
                                  :key #'first :test #'eq)))
                  (if spec
                      (third spec)
                      (error "BUG: Default initarg ~a disappeared" ',key))))
          collect key into new-keys
          collect bind into new-params
          collect `(,bind (funcall ,function-form)) into bindings
          finally (return (values (append keys new-keys)
                                  (append params new-params)
                                  bindings)))))

(defun make-instance-method-keywords (class)
  (loop for method in (nconc
                        (compute-applicable-methods
                         #'allocate-instance (list class))
                        (compute-applicable-methods
                         #'initialize-instance (list (clos:class-prototype class)))
                        (compute-applicable-methods
                         #'shared-initialize (list (clos:class-prototype class) t)))
        for k = (clos::method-keywords method)
        for aok-p = (clos::method-allows-other-keys-p method)
        when aok-p return t else append k))

(defun slot-keywords (class)
  (loop for slotd in (clos:class-slots class)
        append (clos:slot-definition-initargs slotd)))

(defun valid-keywords (class)
  ;; NOTE: Some keywords may be in the return list twice. It doesn't matter.
  ;; Extremely pedantic note: CLHS 7.1.2 specifically says :allow-other-keys is a
  ;; valid initialization argument, so :allow-other-keys nil is still a valid argument.
  (let ((method-keywords (make-instance-method-keywords class))
        (slot-keywords (slot-keywords class)))
    (cons :allow-other-keys
          (if (eq method-keywords t)
              slot-keywords
              (append method-keywords slot-keywords)))))

(defun bad-initargs (class keys)
  (loop with valid-keywords = (valid-keywords class)
        for key in keys
        unless (member key valid-keywords)
          collect key))
