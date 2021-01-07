(in-package #:static-gfs)

;;; update-instance-for-different-class

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

;;; change-class

(defun default-change-class-form (from-class to-class iform keys params)
  (declare (ignore from-class))
  `(locally (declare (notinline change-class))
     (change-class ,iform ,to-class ,@(reconstruct-arguments keys params))))

;; Collect slotnames that are instance-allocated in the TO class that do not
;; name slots in the FROM class.
(defun added-instance-slotnames (old-slotds new-slotds)
  (loop for new-slotd in new-slotds
        for new-slotd-name = (clos:slot-definition-name new-slotd)
        when (and (eq (clos:slot-definition-allocation new-slotd)
                      :instance)
                  (not (member new-slotd-name old-slotds
                               :key #'clos:slot-definition-name
                               :test #'eq)))
          collect new-slotd-name))

;; Collect slotnames that are instance-allocated in the TO class that also name
;; slots in the FROM class. Per 7.2.1, their values must be transferred even
;; if the source slots are not instance allocated.
(defun shared-instance-slotnames (old-slotds new-slotds)
  (loop for new-slotd in new-slotds
        for new-slotd-name = (clos:slot-definition-name new-slotd)
        when (and (eq (clos:slot-definition-allocation new-slotd)
                      :instance)
                  (member new-slotd-name old-slotds
                          :key #'clos:slot-definition-name
                          :test #'eq))
          collect new-slotd-name))

(defun new-rack-initializer (old-rack-form new-rack-form old-slotds new-slotds
                             from-class shared-slotnames)
  `(setf
    ,@(loop for slotn in shared-slotnames
            for old-slotd = (find slotn old-slotds
                                  :key #'clos:slot-definition-name
                                  :test #'eq)
            for old-slot-value-form
              = (ecase (clos:slot-definition-allocation old-slotd)
                  ((:instance)
                   `(si:rack-ref ,old-rack-form
                                 ,(clos:slot-definition-location old-slotd)))
                  ((:class)
                   `(car (load-time-value
                          (clos:slot-definition-location
                           ,(slotd-form from-class old-slotd))))))
            for new-slotd = (find slotn new-slotds
                                  :key #'clos:slot-definition-name
                                  :test #'eq)
            for new-slotd-location
              = (clos:slot-definition-location new-slotd)
            ;; Old slot locations may be for class slots, so we need
            ;; to do some quotation.
            ;; Note that if we want to actually dump forms, rather than
            ;; compile/load them on the spot, we'll need to do fancier things
            ;; like those done in shared-initialize.
            collect `(si:rack-ref ,new-rack-form ',new-slotd-location)
            collect old-slot-value-form)))

(defun standard-change-class-form (from-class to-class iform keys params)
  (let* ((from-slotds (clos:class-slots from-class))
         (to-slotds (clos:class-slots to-class))
         (added-slotnames (added-instance-slotnames from-slotds to-slotds))
         (shared-slotnames (shared-instance-slotnames from-slotds to-slotds)))
    `(let* ((old-rack (core:instance-rack ,iform))
            (new-rack (core:make-rack
                       ',(clos::class-size to-class)
                       ',(clos:class-slots to-class)
                       ',(core:class-stamp-for-instances to-class)
                       (core:unbound)))
            (copy (core:allocate-raw-instance ,from-class old-rack)))
       ,(new-rack-initializer 'old-rack 'new-rack from-slotds to-slotds
                              from-class shared-slotnames)
       (setf (core:instance-rack ,iform) new-rack
             (core:instance-class ,iform) ,to-class)
       ,(uifdc-form from-class to-class 'copy iform
                    added-slotnames keys params)
       instance)))

(defun change-class-form (from-class to-class iform keys params)
  (let ((patch-list
          ;; TODO? change-class on funcallables
          (list
           (cons (find-method #'change-class nil
                              (mapcar #'find-class
                                      '(standard-object standard-class)))
                 #'standard-change-class-form)))
        (methods (compute-applicable-methods
                  #'change-class
                  (list (clos:class-prototype from-class) to-class))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'change-class methods (list from-class to-class iform keys params)
         patch-list (list* iform to-class (reconstruct-arguments keys params)))
        (default-change-class-form from-class to-class iform keys params))))

(defclass class-changer ()
  ((%call-history :accessor changer-call-history)
   (%to-classd :initarg :to :reader changer-to)
   (%keys-params :initarg :keys-params :reader changer-keys-params))
  (:metaclass clos:funcallable-standard-class))

(defun class-changer-name (to-class keys)
  (make-symbol (format nil "CLASS-CHANGER-~a~{-~a~}"
                       (class-name to-class) keys)))

(defun generate-class-changer-function (class-changer)
  (let* ((call-history (changer-call-history class-changer))
         (to-classd (changer-to class-changer))
         (to-class (etypecase to-classd
                     (symbol (find-class to-classd))
                     (class to-classd)))
         (kp (changer-keys-params class-changer))
         (keys (car kp)) (params (cdr kp)))
    (unless (clos:class-finalized-p to-class)
      ;; As in computer-constructor.lisp, this will signal an error if
      ;; the inheritance cannot yet be finalized. That's okay because we're
      ;; only here if we're in the middle of a change-class call which would
      ;; signal anyway.
      (clos:finalize-inheritance to-class))
    `(lambda (instance ,@params)
       (declare (core:lambda-name ,(class-changer-name to-class keys)))
       (clos::discriminate (instance)
           (class-changer-miss ,class-changer instance ,@params)
         ,@(loop for (from-class . form) in call-history
                 collect `(((,from-class)) ,form))))))

(defun update-class-changer-function (changer)
  (clos:set-funcallable-instance-function
   changer
   (cmp:bclasp-compile nil (generate-class-changer-function changer))))

(defun class-changer-miss (changer instance &rest args)
  (when (clos::maybe-update-instance instance)
    (return-from class-changer-miss
      (apply changer instance args)))
  (let* ((from-class (class-of instance))
         (to-classd (changer-to changer))
         (to-class (etypecase to-classd
                     (symbol (find-class to-classd))
                     (class to-classd)))
         (kp (changer-keys-params changer))
         (keys (car kp)) (params (cdr kp))
         (form (change-class-form from-class to-class 'instance keys params)))
    (loop for old-ch = (changer-call-history changer)
          for new-ch = (if (assoc from-class old-ch)
                           ;; added by another thread: quit
                           (return)
                           (acons from-class form old-ch))
          for c = (mp:cas (slot-value changer '%call-history)
                          old-ch new-ch)
          until (eq c old-ch))
    (update-class-changer-function changer)
    (apply changer instance args)))

(defun invalidated-changer (changer)
  (lambda (instance &rest args)
    (declare (core:lambda-name invalidated-changer))
    (update-class-changer-function changer)
    (apply changer instance args)))

(defun invalidate-changer (changer)
  (setf (changer-call-history changer) nil)
  (clos:set-funcallable-instance-function changer
                                          (invalidated-changer changer)))

(defun make-class-changer (to-classd keys)
  (let* ((params (loop for key in keys collect (make-symbol (symbol-name key))))
         (keys-params (cons keys params))
         (changer
           (make-instance 'class-changer
             :to to-classd :keys-params keys-params)))
    (invalidate-changer changer)
    changer))

;; Cut any CH entries for a subclass of the given class, and then recompute
;; the function.
(defun changer-maybe-cut-wrt-class (changer invalidating-class)
  (flet ((test (class) (core:subclassp class invalidating-class)))
    (loop for old-ch = (changer-call-history changer)
          for new-ch
            = (if (assoc-if #'test old-ch)
                  (remove-if #'test old-ch :key #'car)
                  ;; No entries to remove
                  (return-from changer-maybe-cut-wrt-class nil))
          for c = (mp:cas (slot-value changer '%call-history) old-ch new-ch)
          until (eq c old-ch)))
  (update-class-changer-function changer)
  t)

(defclass changer-updater () ())
(let ((updater (make-instance 'changer-updater)))
  (clos:add-dependent #'change-class updater)
  (clos:add-dependent #'update-instance-for-different-class updater)
  (clos:add-dependent #'shared-initialize updater))

(defun invalidate-all-changers () (map-all-changers #'invalidate-changer))

(defmethod clos:update-dependent
    ((f (eql #'change-class)) (updater changer-updater) &rest initargs)
  (declare (ignore initargs))
  ;; KLUDGE: Blunt
  (invalidate-all-changers))

(defmethod clos:update-dependent
    ((f (eql #'update-instance-for-different-class))
     (updater changer-updater) &rest initargs)
  (declare (ignore initargs))
  (invalidate-all-changers))

(defmethod clos:update-dependent
    ((f (eql #'shared-initialize)) (updater changer-updater) &rest initargs)
  (declare (ignore initargs))
  (invalidate-all-changers))

(define-compiler-macro change-class
    (&whole form instancef class-designatorf &rest initargs &environment env)
  (let ((class-designator
          (and (constantp class-designatorf env)
               (ext:constant-form-value class-designatorf env))))
    (multiple-value-bind (keys syms bindings validp)
        (extract initargs env)
      (cond ((not validp)
             (if (and class-designator (symbolp class-designator))
                 `(locally (declare (notinline change-class))
                    (change-class ,instancef (find-class ',class-designator)
                                  ,@initargs))
                 form))
            (class-designator
             (let ((instanceg (gensym "INSTANCE"))
                   (cellg (gensym "CLASS-CHANGER")))
               `(let ((,cellg
                        (load-time-value
                         (ensure-class-changer ',class-designator ',keys)))
                      (,instanceg ,instancef)
                      ,@bindings)
                  (funcall ,cellg ,instanceg ,@syms))))
            (t
             (let ((instanceg (gensym "INSTANCE"))
                   (cellg (gensym "CLASS-CHANGER")))
               `(let ((,cellg
                        (ensure-class-changer ,class-designatorf ',keys))
                      (,instanceg ,instancef)
                      ,@bindings)
                  (funcall ,cellg ,instanceg ,@syms))))))))
