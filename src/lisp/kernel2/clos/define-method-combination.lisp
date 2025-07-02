#-building-clasp(in-package #:cross-clasp.clasp.clos)
#+building-clasp(in-package #:clos)

;;;; This file is compiled/loaded both by the host and the target.
;;;; Tricky bit with that: the host CLOS package shadows DEFINE-METHOD-COMBINATION.
;;;; The target package does not. But this should work out, as the loader doesn't
;;;; know or care about packaging - it will just intern D-M-C in CLOS and whether
;;;; that puts it in CL or not depends on the target package.

(defmacro define-simple-method-combination (name &key documentation
					 identity-with-one-argument
					 (operator name))
  `(define-complex-method-combination
     ,name (&optional (order :MOST-SPECIFIC-FIRST))
     ((around (:AROUND))
      (principal (,name) :REQUIRED t))
     ,documentation
     (let ((main-effective-method
             (list* ',operator (mapcar (lambda (x)
                                         (list 'call-method x ()))
				       (if (eql order :MOST-SPECIFIC-LAST)
					   (reverse principal)
					   principal)))))
       (cond (around
              (list 'call-method (first around)
                    (append (rest around)
                            (list (list 'make-method main-effective-method)))))
             ,@(if identity-with-one-argument
                   `(((null (rest principal))
                      (second main-effective-method))))
             (t main-effective-method)))))

(defun parse-complex-dmc-body (body)
  (loop with argsp = nil with gfp = nil
        with args = nil with gf = nil
        for rbody on body
        for first = (first rbody)
        if (and (consp first) (eq (first first) :arguments)
             (not argsp))
          do (setf argsp t args (rest first))
        else if (and (consp first) (eq (first first) :generic-function)
                  (cdr (rest first)) (null (cddr first))
                  (symbolp (second first)) (not gfp))
               do (setf gfp t gf (second first))
        else do (loop-finish)
        finally (return (values args argsp gf gfp rbody))))

(defmacro define-complex-method-combination (name (&rest lambda-list)
                                             (&rest method-groups)
                                             &rest body)
  (unless (symbolp name)
    (error "Method combination name must be a symbol, but got ~s" name))
  (multiple-value-bind (args-lambda-list argsp gf-symbol gfp body)
      (parse-complex-dmc-body body)
    (declare (ignore args-lambda-list gfp))
    (when argsp
      (warn "Option :ARGUMENTS is not supported in DEFINE-METHOD-COMBINATION.")
      (return-from define-complex-method-combination
        `(error "Option :ARGUMENTS is not supported in DEFINE-METHOD-COMBINATION.")))
    (let ((gf-symbol (or gf-symbol (gensym "GENERIC-FUNCTION")))
          (group-names '()) (group-checks '()) (group-after '()))
      (dolist (group method-groups)
        (destructuring-bind (group-name predicate &key description
                                                    (order :most-specific-first)
                                                    (required nil))
	    group
          (declare (ignore description)) ; FIXME?
	  (if (symbolp group-name)
	      (push group-name group-names)
	      (error "Method combination method group name must be a symbol, but got ~s" group-name))
	  (let ((condition
                  (cond ((eql predicate '*) 'T)
                        ((null predicate) `(null .method-qualifiers.))
                        ((symbolp predicate)
                         `(,predicate .method-qualifiers.))
                        ((consp predicate)
                         (let* ((q (last predicate 0))
                                (p (copy-list (butlast predicate 0))))
                           (when (every #'symbolp p)
                             (if (eql q '*)
                                 `(every #'equal ',p .method-qualifiers.)
                                 `(equal ',p .method-qualifiers.)))))
                        (t (error "Invalid method group predicate: ~s" predicate)))))
	    (push `(,condition (push .method. ,group-name)) group-checks))
	  (when required
	    (push `(unless ,group-name
                     ;; Effective methods can be computed in other situations than being
                     ;; about to call them. As such, compute-effective-method should not
                     ;; signal an error unless the computation is impossible. Lacking a
                     ;; required method is by contrast a problem that only needs to be
                     ;; signaled when the function is actually being called. So we return
                     ;; an error form. ...but because we want an independent function for
                     ;; the dtree interpreter, we return something specially recognizable
                     ;; by compute-outcome, so the generic function etc. can be hooked up.
                     (return-from ,name '(%magic-no-required-method ,group-name)))
		  group-after))
	  (case order
	    (:most-specific-first
	     (push `(setf ,group-name (nreverse ,group-name)) group-after))
	    (:most-specific-last)
	    (otherwise
             (let ((order-var (gensym)))
               (setf group-names (append group-names (list (list order-var order)))
                     group-after (list* `(when (eq ,order-var :most-specific-first)
                                           (setf ,group-name (nreverse ,group-name)))
                                        group-after)))))))
      `(install-method-combination
        ',name
        (lambda (,gf-symbol .methods-list. ,@lambda-list)
          (declare (core:lambda-name ,name)
                   (ignorable ,gf-symbol))
          (block ,name 
            (let (,@group-names)
              (dolist (.method. .methods-list.)
                (let ((.method-qualifiers. (method-qualifiers .method.)))
                  (cond ,@(nreverse group-checks)
                        (t (invalid-method-error .method.
                                                 "Method qualifiers ~S are not allowed in the method ~
			      combination ~S." .method-qualifiers. ',name)))))
              ,@group-after
              ,@body)))))))

(defmacro define-method-combination (name &body body)
  (if (and body (listp (first body)))
      `(define-complex-method-combination ,name ,@body)
      `(define-simple-method-combination ,name ,@body)))
