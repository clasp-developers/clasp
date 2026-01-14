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

#+clasp
(defmethod make-load-form ((object outcome) &optional env)
  (maclina.compile-file:make-load-form maclina.machine:*client* object env))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:client)
                                                (object compiler-class)
                                                &optional env)
  (declare (ignore env))
  `(find-class ',(name object)))

#+clasp
(defmethod make-load-form ((object compiler-class) &optional env)
  (maclina.compile-file:make-load-form maclina.machine:*client* object env))

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

#+clasp
(defmethod make-load-form ((object compiler-method) &optional env)
  (maclina.compile-file:make-load-form maclina.machine:*client* object env))

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

#+clasp
(defmethod make-load-form ((object effective-accessor) &optional env)
  (maclina.compile-file:make-load-form maclina.machine:*client* object env))

;;; The following method is for objects that end up in cfasls but not fasls,
;;; so we don't have to be nearly as scrupulous - they execute in the compiler's
;;; environment, not the target primitive clasp, and use the ct-client.
(defun %make-load-form-saving-slots (object &key (slot-names nil snp)
                                              environment)
  (declare (ignore environment))
  (let* ((class (class-of object))
         (all-slots (mop:class-slots class))
         (slot-names
           (if snp slot-names (mapcar #'mop:slot-definition-name all-slots))))
    ;; sanity check that slots exist
    (when snp
      (loop for sn in slot-names
            unless (find sn all-slots :key #'mop:slot-definition-name)
              collect sn into broken
            finally (when broken
                      (error "BUG: Missing slots: ~s" broken))))
    ;; dump
    (values `(allocate-instance (find-class ',(class-name class)))
            `(progn ,@(loop for slot-name in slot-names
                            if (slot-boundp object slot-name)
                              collect `(setf (slot-value ,object ',slot-name)
                                             ',(slot-value object slot-name))
                            else collect `(slot-makunbound ,object ',slot-name))))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:ct-client)
                                                (object compiler-metaobject)
                                                &optional env)
  (declare (ignore env))
  (%make-load-form-saving-slots object))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:ct-client)
                                                (object compiler-generic)
                                                &optional env)
  (declare (ignore env))
  (multiple-value-bind (create init) (call-next-method)
    (values `(or (gf-info ',(name object)) ,create)
            `(unless (gf-info ',(name object)) ,init))))

(defmethod maclina.compile-file:make-load-form ((client cross-clasp:ct-client)
                                                (object compiler-class)
                                                &optional env)
  (declare (ignore env))
  (multiple-value-bind (create init) (call-next-method)
    (values `(or (find-compiler-class ',(name object) nil) ,create)
            `(unless (find-compiler-class ',(name object) nil) ,init))))
