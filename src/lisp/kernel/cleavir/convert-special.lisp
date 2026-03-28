(in-package :clasp-cleavir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dealing with type checks.

;;; Convert a type spec to something acceptable to typep.
;;; Which just means reducing (function ...) to function.
;;; TODO: Move to cleavir?
(defun discrimination-type (ctype)
  (etypecase ctype
    ((or symbol class) ctype)
    (cons
     (case (first ctype)
       ((function) 'function)
       ((and complex cons not or)
        `(,(first ctype)
          ,@(mapcar #'discrimination-type (rest ctype))))
       (t ctype)))))

(defun make-general-type-check-fun (ctype system)
  (let* ((req (ctype:values-required ctype system))
         (lreq (length req))
         (opt (ctype:values-optional ctype system))
         (lopt (length opt))
         (rest (ctype:values-rest ctype system))
         (rest-top-p (ctype:top-p rest system))
         (rest-bot-p (ctype:bottom-p rest system))
         (reqvars (loop repeat lreq collect (gensym "CHECKED")))
         (reqp (loop repeat lreq collect (gensym "CHECKED-PRESENT-P")))
         (optvars (loop repeat lopt collect (gensym "CHECKED")))
         (optp (loop repeat lopt collect (gensym "CHECKED-PRESENT-P")))
         (restvar (gensym "REST")))
    (flet ((one (var type)
             (let ((dtype (discrimination-type type)))
               `(unless (typep ,var ',dtype)
                  ;; We use the discrimination type in the error as well.
                  ;; The ANSI tests expect to be able to use it as a discrimination
                  ;; type, and we might as well comply.
                  (error 'type-error :datum ,var :expected-type ',dtype))))
           (mash (var -p) `(,var nil ,-p)))
      `(lambda (&optional ,@(mapcar #'mash reqvars reqp)
                  ,@(mapcar #'mash optvars optp)
                &rest ,restvar)
         (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                  (ignorable ,@reqp)
                  (optimize (safety 0)))
         ,@(loop for reqv in reqvars
                 for -p in reqp
                 for ty in req
                 ;; If the type doesn't include NIL, we can skip checking -p since
                 ;; the type check will be (typep nil whatever) which will fail.
                 unless (ctype:disjointp ty 'null system)
                   ;; FIXME: Better error
                   collect `(unless ,-p
                              (error "Required parameter of type ~s not provided"
                                     ',ty))
                 collect (one reqv ty))
         ,@(loop for optv in optvars
                 for -p in optp
                 for ty in opt
                 ;; Our default is NIL, so if the type includes NIL we don't need
                 ;; to check the presence, but otherwise we do.
                 collect (if (ctype:disjointp ty 'null system)
                             `(when ,-p ,(one optv ty))
                             (one optv ty)))
         ,@(cond (rest-top-p nil)
                 (rest-bot-p
                  `((when ,restvar
                      (error 'type-error :datum (first ,restvar) :expected-type 'nil))))
                 (t
                  (let ((rv (gensym "CHECKED")))
                    `((loop for ,rv in ,restvar do ,(one rv rest))))))
         ;; Check successful, now return the values
         ,(if rest-bot-p
              `(cond ,@(loop for optvs on (reverse optvars) for -p in (reverse optp)
                             collect `(,-p (values ,@reqvars ,@(reverse optvs))))
                     (t (values ,@reqvars)))
              `(multiple-value-call #'values
                 ,@reqvars
                 (cond
                   ,@(loop for optvs on (reverse optvars) for -p in (reverse optp)
                           collect `(,-p (values ,@(reverse optvs))))
                   (t (values)))
                 (values-list ,restvar)))))))

(defun make-type-check-fun (context ctype system)
  (flet ((one (var type)
           (let ((dtype (discrimination-type type)))
             `(unless (typep ,var ',dtype)
                (error 'type-error :datum ,var :expected-type ',dtype)))))
    (ecase context
      ((:variable)
       (let ((var (gensym "CHECKED")))
         `(lambda (,var)
            (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                     (optimize (safety 0)))
            ,(one var ctype)
            ,var)))
      ((:setq :argument)
       (let ((var (gensym "CHECKED")) (ignore (gensym "IGNORE")))
         `(lambda (&optional ,var &rest ,ignore)
            (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                     (ignore ,ignore) (optimize (safety 0)))
            ,(one var ctype)
            ,var)))
      ((:the :return)
       (make-general-type-check-fun ctype system)))))

(defun values-top-p (ctype system)
  (and (null (ctype:values-required ctype system))
       (loop for ct in (ctype:values-optional ctype system)
             always (ctype:top-p ct system))
       (ctype:top-p (ctype:values-rest ctype system) system)))

(defun values-bottom-p (ctype system)
  (some (lambda (ct) (ctype:bottom-p ct system))
        (ctype:values-required ctype system)))

(defun insert-type-checks-level (policy context)
  (policy:policy-value
   policy
   (ecase context
     ((:the :variable :setq) 'type-check-the)
     ((:argument) 'type-check-ftype-arguments)
     ((:return) 'type-check-ftype-return-values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enhance source locations.

;;; Called by ext::special-operator-source-locations in source-location.lisp
(defun special-operator-source-locations (name)
  (declare (ignore name))
  nil ; FIXME
  #+(or)
  ;; We check for explicit specialization rather than using
  ;; compute-applicable-methods so that general around methods etc. are not
  ;; included; they're not really germane to the particular operator.
  (ignore-errors
   (mapcan #'ext::method-source-location
           (remove-if-not
            (lambda (method)
              (let ((spec (first (clos:method-specializers method))))
                (and (typep spec 'clos:eql-specializer)
                     (eq (clos:eql-specializer-object spec) name))))
            (clos:generic-function-methods
             #'cleavir-cst-to-ast:convert-special)))))
