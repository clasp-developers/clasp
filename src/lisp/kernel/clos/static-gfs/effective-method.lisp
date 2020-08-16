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
            finally (return (values primary before (nreverse after))))
    (assert (= (length primary) 1))
    (let ((prim (first primary)))
      (if (and (null before) (null after))
          (patch-method prim patch-args patch-list)
          (flet ((apply-method-form (method)
                   `(clos:apply-method ,method () ,@argument-forms nil)))
            `(multiple-value-prog1
                 (progn
                   ,@(mapcar #'apply-method-form before)
                   ,(patch-method prim patch-args patch-list))
               ,@(mapcar #'apply-method-form after)))))))
