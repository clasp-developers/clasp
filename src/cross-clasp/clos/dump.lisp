(in-package #:cross-clasp.clasp.clos)

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object outcome)
                                                &optional env)
  (declare (ignore env))
  (let* ((form (form object))
         (ep (and (consp form) (eq (first form) 'call-method)
               (typep (second form) 'effective-accessor)))
         (oclass (cond ((not ep) 'effective-method-outcome)
                       ((typep (second form) 'effective-reader)
                        'optimized-slot-reader)
                       ((typep (second form) 'effective-writer)
                        'optimized-slot-writer)
                       (t (error "Unknown method class in ~s" form)))))
    (values `(early-allocate-instance ,oclass)
            `(early-initialize-instance
              ,oclass ,object
              :methods '(,@(methods object))
              ,@(if ep
                    `(:index ,(location (second form))
                      :slot-name ',(name (slot (original (second form))))
                      :class ,(etypecase (second form)
                                (effective-reader
                                 (first (specializers (second form))))
                                (effective-writer
                                 (second (specializers (second form))))))
                    `(:function ,(let* ((gf (gf (first (methods object))))
                                        (req (required-parameters gf))
                                        (rest (if (restp gf)
                                                  (gensym "REST")
                                                  nil))
                                        (ll (if rest
                                                `(,@req &rest ,rest)
                                                req)))
                                   `(lambda ,ll
                                      (with-effective-method-parameters
                                          (,@req ,rest)
                                        ,(form object))))
                      :form ',form))))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-class)
                                                &optional env)
  (declare (ignore env))
  `(find-class ',(name object)))

;;; method dumping is based on the premises that
;;; a) by the time they appear literally, they will already be loaded into the
;;;    generic function, so we just need to grab them
;;; b) EXCEPT for effective accessors, which will not yet exist. But their
;;;    direct slots will exist, so they must be installed.
;;; c) any one effective accessor will only be dumped in one file. Otherwise
;;;    we might get duplicates, which I think is harmless but dumb.
;;; d) slots also already exist.

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-method)
                                                &optional env)
  (declare (ignore env))
  (let* ((gf (gf object))
         (pos (position object (methods gf))))
    (assert pos () "Method on ~s for ~s ~s does not exist"
            (name gf) (qualifiers object) (mapcar #'name (specializers object)))
    `(with-early-accessors (standard-generic-function)
       (elt (generic-function-methods (fdefinition ',(name gf))) ,pos))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object effective-accessor)
                                                &optional env)
  (declare (ignore env))
  (multiple-value-bind (mclass class-getter cache)
      (etypecase object
        (effective-reader (values 'effective-reader-method
                                  #'first
                                  '%effective-readers))
        (effective-writer (values 'effective-writer-method
                                  #'second
                                  '%effective-writers)))
    (let* ((original (original object))
           (class (funcall class-getter (specializers original)))
           ;; we dump the pos form ourselves rather than rely on m-l-f
           ;; because we need to look up the slotd in its class, and the
           ;; slotd doesn't know what its class is.
           (dslot (slot original))
           (dslotpos (position dslot (mop:class-direct-slots class))))
      (assert dslotpos () "Slot ~s is not present in its class ~s"
              (name dslot) (name class))
      (values `(early-allocate-instance ,mclass)
              `(progn
                 (early-initialize-instance ,mclass ,object
                   :original ',original
                   :location ',(location object))
                 (with-early-accessors (std-class standard-direct-slot-definition)
                   (push (cons ',(location object) ,object)
                         (,cache (elt (class-direct-slots ,class)
                                      ,dslotpos)))))))))
