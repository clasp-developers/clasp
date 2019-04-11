;;; Optimization of cons functions
;;; No further relation to ECL file of similar name

(in-package #:cmp)

(defconstant +nthcdr-inline-limit+ 8) ; totally arbitrary

(define-compiler-macro nthcdr (&whole whole index list &environment env)
  (if (constantp index env)
      (let ((i (ext:constant-form-value index env)))
        (if (and (integerp i)
                 (<= 0 i)
                 (< i +nthcdr-inline-limit+))
            (loop for result = list then `(cdr ,result)
                  repeat i
                  finally (return result))
            ;; could do optimization notes
            whole))
      whole))

;;; This is of course basically an inline definition, but we do it at this level
;;; so that the nthcdr can have its compiler macro used (which it wouldn't be, with ASTs)
(define-compiler-macro nth (index list)
  `(car (nthcdr ,index ,list)))

(defmacro do-in-list ((%elt %sublist list &rest output) &body body)
  `(do* ((,%sublist ,list (cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let ((,%elt (car (the cons ,%sublist))))
       ,@body)))

;;; TODO: Avoid iteration for constant list (but watch out for growth)

;;;
;;; MEMBER
;;;

(defun expand-member (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (two-arg-test-parse-args 'member sequence-args :start-end nil :environment env)
    ;; When having complex arguments (:allow-other-keys, etc)
    ;; we just give up.
    (when (null key-function)
      (return-from expand-member nil))
    (si::with-unique-names (%value %sublist %elt)
      `(let ((,%value ,value)
             ,@init)
         (do-in-list (,%elt ,%sublist ,list)
           (when ,(funcall test-function %value
                           (funcall key-function %elt))
             (return ,%sublist)))))))

(define-compiler-macro member (&whole whole value list &rest sequence-args &environment env)
  ;; FIXME: pay attention to policy, e.g. don't inline for high SPACE.
  (or (apply #'expand-member env (rest whole))
      whole))

;;;
;;; ASSOC
;;;

(defun expand-assoc (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (two-arg-test-parse-args 'assoc sequence-args :start-end nil :environment env)
    (when test-function
      (si::with-unique-names (%value %sublist %elt %car)
        `(let ((,%value ,value)
               ,@init)
           (do-in-list (,%elt ,%sublist ,list)
             (when ,%elt
               (let ((,%car (car (the cons ,%elt))))
                 (when ,(funcall test-function %value
                                 (funcall key-function %car))
                   (return ,%elt))))))))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args &environment env)
  (or (apply #'expand-assoc env (rest whole))
      whole))

;;;
;;; ADJOIN
;;;

(defun expand-adjoin (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init
                        key-flag test-flag test)
      (two-arg-test-parse-args 'adjoin sequence-args :start-end nil :environment env)
    (when test-function
      (si::with-unique-names
	  (%value %sublist %elt %car %list %value-after-key-function-)
	`(let ((,%value ,value)
	       (,%list ,list)
	       ,@init)
	   (let ((,%value-after-key-function- ,(funcall key-function %value)))
	     (do-in-list (,%elt ,%sublist ,%list (cons ,%value ,%list))
	       (when ,(funcall test-function %value-after-key-function- 
			       (funcall key-function %elt))
		 (return ,%list)))))))))

(define-compiler-macro adjoin (&whole whole value list &rest sequence-args &environment env)
  (or (apply #'expand-adjoin env (rest whole))
      whole))
