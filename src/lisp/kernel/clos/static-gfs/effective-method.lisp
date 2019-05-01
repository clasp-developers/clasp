(in-package #:static-gfs)

;;; This stuff constructs a form that is like an effective method function,
;;; except some method calls may be replaced by forms.
;;; These forms are produced using the PATCH-LIST, an alist of method->function.
;;; When a CALL-METHOD for a given method would be made, instead the function
;;; is called (along with some user parameters).

;;; :around and other primary methods could change the parameters passed to
;;; a known method, so they invalidate the whole project.
;;; Technically, we could allow LESS-specific primary methods, which would be
;;; called by whatever method we're worrying about... but given we're doing
;;; this for standard functions there's no chance of that.
;;; TODO?: If we tracked whether methods use complex call-next-method
;;; (i.e., anything with it beyond (call-next-method)) we could work with
;;; primary and :around methods as long as they didn't use complex c-n-m.

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))
(defun unknown-primary-method-p (method patch-list)
  (and (null (method-qualifiers method))
       (not (assoc method patch-list :test #'eq))))

(defun can-static-effective-method-p (applicable-methods patch-list)
  (loop for method in applicable-methods
        never (or (around-method-p method)
                  (unknown-primary-method-p method patch-list))))

;; dumb helpers
(defun method-form (method)
  ;; Hoo boy.
  `(find-method
    #',(clos:generic-function-name (clos:method-generic-function method))
    ',(method-qualifiers method)
    ;; EQL specializers are not dumpable. Let's be paranoid.
    (list ,@(loop for spec in (clos:method-specializers method)
                  collect (etypecase spec
                            (class spec)
                            (clos:eql-specializer
                             `(clos:intern-eql-specializer
                               ',(clos:eql-specializer-object spec))))))
    t))

(defun method-function-form (method)
  `(load-time-value (clos:method-function ,(method-form method)) t))

(defun call-method-form (method)
  `(funcall ,(method-function-form method) .arguments. nil))

(defun patch-method (method patch-args patch-list)
  (let ((patch (assoc method patch-list)))
    (if patch
        (apply (cdr patch) patch-args)
        (error "BUG: Missing patch for ~a" method))))

(defun static-effective-method (gf applicable-methods patch-args patch-list
                                argument-forms)
  (multiple-value-bind (primary before after)
      (loop for method in applicable-methods
            for qualifiers = (method-qualifiers method)
            if (equal qualifiers '())
              collect method into primary
            else if (equal qualifiers '(:before))
                   collect method into before
            else if (equal qualifiers '(:after))
                   collect method into after
            finally (return (values primary before after)))
    (assert (= (length primary) 1))
    (let ((prim (first primary)))
      (if (and (null before) (null after))
          (patch-method prim patch-args patch-list)
          ;; (let ((.arguments. (list ,@argument-forms))) ...)
          `((lambda (core:&va-rest .arguments.)
              (multiple-value-prog1
                  (progn
                    ,@(mapcar #'call-method-form before)
                    ,(patch-method prim patch-args patch-list))
                ,@(mapcar #'call-method-form after)))
            ,@argument-forms)))))

#|

This version uses macrolet of call-method. It works, but it's harder to read
the results... and for no good reason, since it only works with the primary
method combination anyway.

To work with general method combinations, all I can think of is having
the CALL-METHOD macro somehow "signal" that we should give up, but
macroexpansion happens too late for that.

(defun static-effective-method (gf applicable-methods patch-args patch-list)
  `(macrolet ((call-method (method &optional nexts)
                (assert (typep method 'method)) ; no make-method
                (let ((patch (assoc method ',patch-list)))
                  (if patch
                      (apply (cdr patch) ',patch-args)
                      ;; do it normally(-ish)
                      `(funcall ,(method-function-form method)
                                .arguments.
                                (list
                                 ,@(loop for next in nexts
                                         do (assert (typep next 'method))
                                            ;; clasp bug: method functions expect
                                            ;; method functions, not methods, as args
                                         collect (method-function-form next))))))))
     ,(clos:compute-effective-method
       gf (clos:generic-function-method-combination gf)
       applicable-methods)))

|#
