(in-package #:static-gfs)

(defun default-uifdc-form (to-class from-iform to-iform
                           added-slotnames keys params)
  (declare (ignore to-class added-slotnames))
  `(locally
       (declare (notinline update-instance-for-different-class))
     (update-instance-for-different-class
      ,from-iform ,to-iform ,@(reconstruct-arguments keys params))))

;; TODO: Proper error
(defun uifdc-initarg-error-form (from-class to-class bad-initargs)
  `(error "Unknown CHANGE-CLASS initialization arguments for ~s to ~s: ~s"
          ',from-class ',to-class ',bad-initargs))

(defun uifdc-method-keywords (from-class to-class added-slotnames)
  (loop for method in (nconc
                        (compute-applicable-methods
                         #'update-instance-for-different-class
                         (list (clos:class-prototype from-class)
                               (clos:class-prototype to-class)))
                        (compute-applicable-methods
                         #'shared-initialize
                         (list (clos:class-prototype to-class)
                               added-slotnames)))
        for k = (clos::method-keywords method)
        for aok-p = (clos::method-allows-other-keys-p method)
        when aok-p return t else append k))

(defun valid-uifdc-keywords (from-class to-class added-slotnames)
  (let ((method-keywords
          (uifdc-method-keywords from-class to-class added-slotnames)))
    (if (eq method-keywords t)
        t
        (append '(:allow-other-keys)
                method-keywords
                (slot-keywords to-class)))))

(defun bad-uifdc-initargs (from-class to-class added-slotnames keys)
  (let ((valid-keywords
          (valid-uifdc-keywords from-class to-class added-slotnames)))
    (if (eq valid-keywords t)
        nil
        (set-difference keys valid-keywords))))

(defun standard-uifdc-form (from-class to-class from-iform to-iform
                            added-slotnames keys params)
  (declare (ignore from-iform))
  `(progn
     ,@(let ((bad-initargs (bad-uifdc-initargs from-class to-class
                                               added-slotnames keys)))
         (unless (null bad-initargs)
           (let ((pos (position :allow-other-keys keys :test #'eq)))
             (if pos
                 (let ((aok-param (nth pos params)))
                   `((unless ,aok-param
                       ,(uifdc-initarg-error-form
                         from-class to-class bad-initargs))))
                 (return-from standard-uifdc-form
                   (uifdc-initarg-error-form
                    from-class to-class bad-initargs))))))
     ,(shared-initialize-form to-class added-slotnames to-iform keys params)))

(defun uifdc-form (from-class to-class from-iform to-iform
                   added-slotnames keys params)
  (let ((patch-list
          (list
           (cons (find-method #'update-instance-for-different-class
                              nil (mapcar #'find-class
                                          '(standard-object standard-object)))
                 #'standard-uifdc-form)))
        (methods (compute-applicable-methods
                  #'update-instance-for-different-class
                  (list (clos:class-prototype from-class)
                        (clos:class-prototype to-class)))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'update-instance-for-different-class
         methods
         (list from-class to-class from-iform to-iform
               added-slotnames keys params)
         patch-list
         (list* from-iform to-iform (reconstruct-arguments keys params)))
        (default-uifdc-form to-class from-iform to-iform
                            added-slotnames keys params))))
