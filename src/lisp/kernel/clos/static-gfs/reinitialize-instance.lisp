(in-package "STATIC-GFS")

(defun reinitialize-instance-form (class iform keys params)
  (let ((patch-list
          (list
           (cons (find-method #'reinitialize-instance nil (list (find-class 't)))
                 #'standard-reinitialize-instance-form)))
        (methods (compute-applicable-methods #'reinitialize-instance
                                             (list (clos:class-prototype class)))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'reinitialize-instance methods (list class iform keys params) patch-list
         (list* iform (reconstruct-arguments keys params)))
        (default-reinitialize-instance-form iform keys params))))

(defun default-reinitialize-instance-form (iform keys params)
  `(locally
       (declare (notinline reinitialize-instance))
     (reinitialize-instance ,iform ,@(reconstruct-arguments keys params))))

;; TODO: Proper error
(defun reinitarg-error-form (class bad-reinitargs)
  `(error "Unknown reinitialization arguments for ~s: ~s"
          ',class ',bad-reinitargs))

(defun reinitialize-instance-method-rekeywords (class)
  (loop for method in (nconc
                        (compute-applicable-methods
                         #'reinitialize-instance
                         (list (clos:class-prototype class)))
                        (compute-applicable-methods
                         #'shared-initialize
                         (list (clos:class-prototype class) nil)))
        for k = (clos::method-keywords method)
        for aok-p = (clos::method-allows-other-keys-p method)
        when aok-p return t else append k))

(defun valid-rekeywords (class)
  (let ((method-rekeywords (reinitialize-instance-method-rekeywords class)))
    (if (eq method-rekeywords t)
        t
        (append '(:allow-other-keys)
                method-rekeywords
                (slot-keywords class)))))

(defun bad-reinitargs (class keys)
  (let ((valid-rekeywords (valid-rekeywords class)))
    (if (eq valid-rekeywords t)
        nil
        (set-difference keys valid-rekeywords))))

(defun standard-reinitialize-instance-form (class iform keys params)
  ;; default-initargs are only relevant to instance _creation_ per ANSI 7.1.3,
  ;; so we don't need to worry about that, but we do still need to check
  ;; the validity of initargs.
  `(progn
     ,@(let ((bad-reinitargs (bad-reinitargs class keys)))
         (unless (null bad-reinitargs)
           (let ((pos (position :allow-other-keys keys :test #'eq)))
             (if pos
                 (let ((aok-param (nth pos params)))
                   `((unless ,aok-param
                       ,(reinitarg-error-form class bad-reinitargs))))
                 (return-from standard-reinitialize-instance-form
                   (reinitarg-error-form class bad-reinitargs))))))
     ,(shared-initialize-form class nil iform keys params)))

(defclass reinitializer ()
  ((%call-history :accessor reinitializer-call-history :initform nil)
   (%keys-params :reader reinitializer-keys-params :initarg :keys-params))
  (:metaclass clos:funcallable-standard-class))

(defun make-reinitializer (keys)
  (let* ((params (loop for key in keys collect (make-symbol (symbol-name key))))
         (keys-params (cons keys params))
         (r
           (make-instance 'reinitializer
                          :keys-params keys-params)))
    (update-reinitializer-function r)
    r))

(defun reinitializer-name (keys)
  (make-symbol (with-standard-io-syntax
                 (format nil "REINITIALIZER~{-~a~}" keys))))

(defun generate-reinitializer-function (reinitializer)
  (let* ((call-history (reinitializer-call-history reinitializer))
         (kp (reinitializer-keys-params reinitializer))
         (keys (car kp)) (params (cdr kp)))
    `(lambda (instance ,@params)
       (declare (core:lambda-name ,(reinitializer-name keys)))
       (clos::discriminate (instance)
           (reinitializer-miss ,reinitializer instance ,@params)
         ,@(loop for (class . form) in call-history
                 collect `(((,class)) ,form))))))

(defun update-reinitializer-function (reinitializer)
  (clos:set-funcallable-instance-function
   reinitializer
   #+(or)
   (let ((cmp:*cleavir-compile-hook* nil))
     (coerce (generate-reinitializer-function reinitializer) 'function))
   ;;#+(or)
   (error "BUG: No suitable compiler yet")))

(defun reinitializer-miss (reinitializer instance &rest args)
  (when (clos::maybe-update-instance instance)
    ;; Instance has been updated - go around again.
    (return-from reinitializer-miss
      (apply reinitializer instance args)))
  (let* ((class (class-of instance))
         (kp (reinitializer-keys-params reinitializer))
         (keys (car kp)) (params (cdr kp))
         (form (reinitialize-instance-form class 'instance keys params)))
    (mp:atomic-pushnew (cons class form)
                       (slot-value reinitializer '%call-history)
                       :key #'car)
    (update-reinitializer-function reinitializer)
    (apply reinitializer instance args)))

;; If a class appears in the call history, delete that entry and recompute
;; the initializer.
(defun maybe-invalidate-reinitializer-wrt-classes (reinitializer classes)
  (loop with test = (lambda (class) (member class classes))
        for old-ch = (reinitializer-call-history reinitializer)
        for new-ch
          = (if (assoc-if test old-ch)
                ;; FIXME duplicates effort
                (remove-if test old-ch :key #'car)
                ;; No entries to remove
                (return-from maybe-invalidate-reinitializer-wrt-classes nil))
        for c = (mp:cas (slot-value reinitializer '%call-history) old-ch new-ch)
        until (eq c old-ch))
  (update-reinitializer-function reinitializer)
  t)

;; Wipe a reinitializer's CH entirely and force the function.
(defun wipe-reinitializer (reinitializer)
  (setf (reinitializer-call-history reinitializer) nil)
  (update-reinitializer-function reinitializer)
  t)

(defclass reinitializer-updater () ())
(let ((updater (make-instance 'reinitializer-updater)))
  (clos:add-dependent #'reinitialize-instance updater)
  (clos:add-dependent #'shared-initialize updater))

(defmethod clos:update-dependent
    ((f (eql #'reinitialize-instance)) (updater reinitializer-updater)
     &rest initargs)
  (declare (ignore initargs))
  ;; KLUDGE! What a blunt instrument.
  ;; Better idea: Make another funcallable class for the individual
  ;; i.e. per class clauses in the reinitializer, have the reinitializer call
  ;; those, and have _them_ update. Possibly could even hook that into dispatch
  ;; misses by replacing their instance function appropriately? Hm hm hm.
  ;; Or at least check the relevant classes from the new/removed methods,
  ;; like how it works with make-instance.
  (wipe-all-reinitializers))
(defmethod clos:update-dependent
    ((f (eql #'shared-initialize)) (updater reinitializer-updater)
     &rest initargs)
  (declare (ignore initargs))
  (wipe-all-reinitializers))

;; This is used in finalize-inheritance analogously to
;; invalidate-class-constructors
(defun invalidate-class-reinitializers (class)
  (let ((classes (list class)))
    (maphash (lambda (k reinitializer)
               (declare (ignore k))
               (maybe-invalidate-reinitializer-wrt-classes
                reinitializer classes))
             *reinitializers*)))
#+(or)
(define-compiler-macro reinitialize-instance
    (&whole form instance &rest initargs &environment env)
  (multiple-value-bind (keys syms bindings validp)
      (extract initargs env)
    (if validp
        (let ((isym (gensym "INSTANCE")))
          `(let (;; To preserve evaluation order, the instance must be
                 ;; evaluated before any of the initargs.
                 (,isym ,instance)
                 ,@bindings)
             (funcall (load-time-value (ensure-reinitializer ',keys))
                      ,isym ,@syms)))
        form)))
