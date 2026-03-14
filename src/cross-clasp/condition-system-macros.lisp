(in-package #:cross-clasp)

;;; Kind of a frankenstein of Clasp and common macros here.

(defun restart-clause-pop-keywords-from-clause (clause-forms)
  (let ((things clause-forms) report interactive test)
    (macrolet ((handle-keyword (symbol keyword)
                 (let ((value (gensym "KEYWORD-VALUE")))
                   `(progn
                      (when ,symbol
                        (error "Duplicate ~S in clause ~S." ,keyword clause-forms))
                      (pop things)
                      (let ((,value (pop things)))
                        (unless ,value
                          (error "~S may not be NIL in HANDLER-CLAUSE." ,keyword))
                        (setf ,symbol ,value))))))
      (loop
        (let ((thing (first things)))
          (cond
            ((null (rest things))
             (return (values things report interactive test)))
            ((eq thing :report) (handle-keyword report :report))
            ((eq thing :interactive) (handle-keyword interactive :interactive))
            ((eq thing :test) (handle-keyword test :test))
            (t (return (values things report interactive test)))))))))

;;; FIXME?
(defmacro ext:with-current-source-form ((&rest forms) &body body)
  `(progn ,@forms ,@body))

(defmacro %handler-bind (bindings &body forms)
  `(let ((core::*handler-clusters*
           (cons (list ,@(mapcar #'(lambda (binding)
                                     (ext:with-current-source-form (binding)
                                       (unless (and (listp binding)
                                                    (= (length binding) 2))
                                         (error
                                          "Ill-formed handler binding ~s."
                                          binding))
                                       `(cons (lambda (condition)
                                                (typep condition ',(car binding)))
                                              ,(cadr binding))))
                                 bindings))
                 core::*handler-clusters*)))
     ,@forms))

(defun munge-restart-case-clause (clause)
  (ext:with-current-source-form (clause)
    (destructuring-bind (name lambda-list . body) clause
      (multiple-value-bind (body report interactive test)
          (restart-clause-pop-keywords-from-clause body)
        (values name lambda-list body
                (nconc
                 (etypecase report
                   (string
                    (list :report-function
                          `(lambda (stream) (write-string ,report stream))))
                   (null nil)
                   ((or symbol (cons (eql lambda)))
                    (list :report-function `#',report)))
                 (if interactive
                     (list :interactive-function `#',interactive)
                     nil)
                 (if test
                     (list :test-function `#',test)
                     nil)))))))

(defun munge-with-condition-restarts-form (original-form env)
  (ext:with-current-source-form (original-form)
    (let ((form (build-macroexpand original-form env)))
      (if (consp form)
          (let* ((name (first form))
                 (condition-form
                   (case name
                     ((signal)
                      `(core::coerce-to-condition ,(second form)
                                                  (list ,@(cddr form))
                                                  'simple-condition
                                                  'signal))
                     ((warn)
                      `(core::coerce-to-condition ,(second form)
                                                  (list ,@(cddr form))
                                                  'simple-warning 'warn))
                     ((error)
                      `(core::coerce-to-condition ,(second form)
                                                  (list ,@(cddr form))
                                                  'simple-error 'error))
                     ((cerror)
                      `(core::coerce-to-condition ,(third form)
                                                  (list ,@(cdddr form))
                                                  'simple-error
                                                  'cerror)))))
            (if condition-form
                (let ((condition-var (gensym "CONDITION")))
                  `(let ((,condition-var ,condition-form))
                     (with-condition-restarts ,condition-var
                         (first core::*restart-clusters*)
                       ,(if (eq name 'cerror)
                            `(cerror ,(second form) ,condition-var)
                            `(,name ,condition-var)))))
                original-form))
          original-form))))

(defmacro %with-condition-restarts (condition restarts &body forms)
  `(let ((core::*condition-restarts* (cons (cons ,condition ,restarts)
				           core::*condition-restarts*)))
    ,@forms))

(defmacro %restart-bind (bindings &body forms)
  `(let ((core::*restart-clusters*
	  (cons (list ,@(mapcar #'(lambda (binding)
				    `(core::make-restart
                                       :NAME     ',(car binding)
                                       :FUNCTION ,(cadr binding)
                                       ,@(cddr binding)))
				bindings))
		core::*restart-clusters*)))
     ,@forms))

(defmacro %restart-case (expression &body clauses &environment env)
  (let* ((block-tag (gensym))
         (temp-var  (gensym))
         ;; This is a gensym in order to avoid leaving cross-clasp symbols
         ;; in macroexpansions.
         (temp-arg  (gensym))
         ;; A list of (name lambda-list body restart-bind-plist)
         (data
           (loop for clause in clauses
                 collect (multiple-value-list
                          (munge-restart-case-clause clause))))
         ;; restart names passed through make-symbol to prevent collisions.
         (gnames (loop for (name) in data
                       for sname = (symbol-name name)
                       collect (make-symbol sname)))
         (expression (munge-with-condition-restarts-form expression env)))
    `(block ,block-tag
       (let ((,temp-var nil))
	 (tagbody
            (return-from ,block-tag
              ;; NOTE: Might need to mess with package locks
              ;; when binding functions of the given names
              ;; but right now clasp and maclina ignore locks
              ;; for local bindings.
              (flet (,@(loop for gname in gnames
                             collect `(,gname (&rest ,temp-arg)
                                        (setq ,temp-var ,temp-arg)
                                        (go ,gname))))
                (declare (dynamic-extent
                          ,@(loop for gname in gnames
                                  collect `(function ,gname))))
                (restart-bind
                    (,@(loop for (name _1 _2 kws) in data
                             for gname in gnames
                             collect `(,name #',gname ,@kws)))
                  ,expression)))
            ,@(loop for gname in gnames
                    for (_ lambda-list body) in data
                    collect gname ; tag
                    collect `(return-from ,block-tag
                               (apply #'(lambda ,lambda-list ,@body)
                                      ,temp-var))))))))


(defmacro %assert (test-form &optional places (datum nil datump) &rest arguments)
  `(core::while (not ,test-form)
     (setf (values ,@places)
           ;; Defined in clos/conditions.lisp
           (core::assert-failure ',test-form ',places (list ,@places)
                                 ;; If DATUM is provided, it must be for a
                                 ;; condition; NIL is not acceptable.
                                 ,(if datump datum nil) ,@arguments))))

(defmacro %check-type (place type &optional type-string)
  (when (and (consp type) (eq 'quote (car type)))
    (error "Quoted type specifier in ~s: ~s"
           'check-type type))
  (let ((aux (gensym)))
    `(let ((,aux ,place))
       (unless (typep ,aux ',type)
         ;; defined in lsp/assert.lisp
	 (setf ,place (core::do-check-type ,aux ',type ',type-string ',place)))
       nil)))
