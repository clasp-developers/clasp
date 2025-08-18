(in-package #:clos)

(defun miss (generic-function &rest arguments)
  (check-gf-argcount generic-function (length arguments))
  ;; Update any invalid instances
  (when (maybe-update-instances arguments)
    (return-from miss (apply generic-function arguments)))
  ;; OK, real miss. Do it.
  (let* ((tracy (generic-function-tracy generic-function))
         (start-time (when tracy (get-internal-real-time))))
    (when tracy
      (trace-miss-start generic-function tracy start-time arguments))
    (multiple-value-bind (outcome updatedp)
        (update-call-history generic-function arguments)
      (when updatedp (force-discriminator generic-function))
      (when tracy
        (trace-miss-end generic-function tracy start-time arguments))
      (perform-outcome outcome arguments))))

;;; FIXME: breaking a few abstractions in this one
;;; Called from atomic expansion defined in cross-clasp.
(defun %gfclass-call-history-loc (gfclass)
  (if (eq gfclass
          (load-time-value (find-class 'standard-generic-function)))
      #.(let ((slots (class-slots (find-class 'standard-generic-function))))
          (or (position 'call-history slots :key #'slot-definition-name)
              (error "BUG: standard-generic-function lacks CALL-HISTORY slot?")))
      (or (position 'call-history (class-slots gfclass) :key #'slot-definition-name)
          (error "BUG?: generic-function lacks CALL-HISTORY slot?"))))
(defun %generic-function-call-history (order generic-function)
  (core:atomic-rack-read order (core:instance-rack generic-function)
                         (%gfclass-call-history-loc (class-of generic-function))))
(defun generic-function-call-history (generic-function)
  (%generic-function-call-history :relaxed generic-function))
(defun (setf %generic-function-call-history) (new order generic-function)
  (core:atomic-rack-write order new
                          (core:instance-rack generic-function)
                          (%gfclass-call-history-loc (class-of generic-function))))
(defun (setf generic-function-call-history) (new generic-function)
  (setf (%generic-function-call-history :relaxed generic-function) new))
(defun cas-generic-function-call-history (order old new generic-function)
  (core:cas-rack order old new (core:instance-rack generic-function)
                 (%gfclass-call-history-loc (class-of generic-function))))

(defun generic-function-tracy (gf)
  (let ((gfclass (class-of gf)))
    (if (eq gfclass
            (load-time-value (find-class 'standard-generic-function)))
        (with-early-accessors (standard-generic-function)
          (mp:atomic (%generic-function-tracy gf)))
        (let* ((slotd (find 'tracy (class-slots gfclass)
                            :key #'slot-definition-name))
               (location (slot-definition-location slotd)))
          (mp:atomic (funcallable-standard-instance-access gf location))))))
;;; used in telemetry
(defun (setf generic-function-tracy) (new gf)
  (let ((gfclass (class-of gf)))
    (if (eq gfclass
            (load-time-value (find-class 'standard-generic-function)))
        (with-early-accessors (standard-generic-function)
          (setf (mp:atomic (%generic-function-tracy gf)) new))
        (let* ((slotd (find 'tracy (class-slots gfclass)
                            :key #'slot-definition-name))
               (location (slot-definition-location slotd)))
          (setf (mp:atomic
                 (funcallable-standard-instance-access gf location))
                new)))))

(defun trace-miss-start (gf tracy start-time arguments)
  (declare (ignore start-time))
  (when (eq (car tracy) :profile-ongoing) ; report
    (format *trace-output* "~&; Dispatch miss: (~a~{ ~s~})~%"
            (core:low-level-standard-generic-function-name gf) arguments)))

(defun trace-miss-end (gf tracy start-time arguments)
  (let ((time-s (/ (float (- (get-internal-real-time) start-time))
                   internal-time-units-per-second)))
    (when (eq (car tracy) :profile-ongoing) ; report
      (format *trace-output* "~&;  ~fs overhead~%" time-s))
    (let (;; dumb hack - atomics don't know about cadr etc
          (info (cdr tracy)))
      (mp:atomic-incf (car info) time-s)
      (mp:atomic-push arguments (cdr info)))))

;;; stupid aliases used by funcallableInstance.cc
(defun dispatch-miss-va (gf vaslist) (apply #'miss gf vaslist))
(defun dispatch-miss (gf &rest args) (apply #'miss gf args))

(defun force-discriminator (generic-function)
  (set-funcallable-instance-function generic-function
                                     (calculate-std-discriminating-function
                                      generic-function)))

(defun calculate-std-discriminating-function (generic-function)
  (bytecode-interpreted-discriminator generic-function))

(defun invalidate-discriminating-function (generic-function)
  (set-funcallable-instance-function
   generic-function
   (fallback-discriminator generic-function)))

(defun fallback-discriminator (generic-function)
  (or (%fallback-discriminator generic-function)
      (setf (%fallback-discriminator generic-function)
            (invalidated-discriminator-closure generic-function))))

(defun invalidated-discriminator-closure (generic-function)
  (lambda (&rest args)
    (declare (core:lambda-name invalidated-discriminator))
    ;; A GF being invalidated is orthogonal from the call history being valid.
    ;; For example, when methods are added or removed, the call history is
    ;; altered accordingly and the GF is invalidated, but the remaining call
    ;; history entries are still valid. All GF invalidation does is force the
    ;; GF to recompute its discriminator (which it does need to do, since e.g.
    ;; its discriminating function predates the method changes).
    ;; So what this closure conceptually needs to do is recompute the
    ;; discriminator and then call it. It's just a mechanism for laziness.
    (if (generic-function-call-history generic-function)
        (progn (force-discriminator generic-function)
               (apply generic-function args))
        ;; If we know the call history is empty, the discriminator will do
        ;; nothing but miss immediately, and MISS will add to the call history
        ;; if possible and implement the GF regardless. So we skip
        ;; computing the discriminator and just call MISS directly.
        (apply #'miss generic-function args))))

(defgeneric compute-discriminating-function (generic-function))

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (fallback-discriminator gf))

(defun update-call-history (generic-function arguments)
  (let (outcome updatedp)
    (flet ((updater (call-history arguments)
             (multiple-value-bind (noutcome new-entries)
                 (miss-info generic-function call-history arguments)
               (setf outcome noutcome)
               (cond ((null new-entries)
                      (setf updatedp nil)
                      call-history)
                     (t (setf updatedp t)
                        (union-entries call-history new-entries))))))
      (mp:atomic-update (generic-function-call-history generic-function)
                        #'updater arguments))
    (values outcome updatedp)))

(defun union-entries (old-call-history new-entries)
  ;; We do this instead of UNION because the new entries can contain duplicates.
  (loop for entry in new-entries
        do (pushnew entry old-call-history
                    :key #'car :test #'specializer-key-match))
  old-call-history)

(defun check-gf-argcount (generic-function nargs)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (when (or (< nargs min) (and max (> nargs max)))
      (error 'core:wrong-number-of-arguments
             :called-function generic-function :given-nargs nargs
             :min-nargs min :max-nargs max))))

;;; returns minimum and maximum number of args allowed as values.
;;; max is NIL if infinite.
(defun generic-function-min-max-args (gf)
  (multiple-value-bind (req opt restvar keyflag) ; rest are irrelevant
      (core:process-lambda-list (generic-function-lambda-list gf) 'function)
    (values (car req) (if (or restvar keyflag) nil (+ (car req) (car opt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update instances. The bulk of the code is in change.lisp,
;;; including the core UPDATE-INSTANCE function.
;;;

;;; Try to update instances; return true iff any updates were performed.
(defun maybe-update-instances (arguments)
  ;; this is SOME, which doesn't exist yet.
  (let ((updatedp nil))
    (dolist (i arguments updatedp)
      (when (maybe-update-instance i)
        (setf updatedp t)))))

(defun maybe-update-instance (instance)
  (when (core:instancep instance)
    (let ((instance-stamp (core:instance-stamp instance))
          ;; circularity note: stamp-for-instances is a generic function,
          ;; but we're calling it on the instance's class rather than the instance.
          ;; Therefore the recursion is ok unless the class _is_ the instance.
          ;; This is only the case for STANDARD-CLASS, which is never obsolete.
          (class-stamp (stamp-for-instances (core:instance-class instance))))
      (unless (= instance-stamp class-stamp)
        (update-instance instance)
        t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Performing outcomes
;;;

(defgeneric perform-outcome (outcome arguments))

(defmethod perform-outcome ((outcome optimized-slot-reader) arguments)
  (let ((value (standard-location-access
                (first arguments) (optimized-slot-accessor-index outcome))))
    (if (core:sl-boundp value)
        value
        (values (slot-unbound (optimized-slot-accessor-class outcome)
                              (first arguments)
                              (optimized-slot-accessor-slot-name outcome))))))

(defmethod perform-outcome ((outcome optimized-slot-writer) arguments)
  (setf (standard-location-access
         (second arguments) (optimized-slot-accessor-index outcome))
        (first arguments)))

(defmethod perform-outcome ((outcome effective-method-outcome) arguments)
  (apply (effective-method-outcome-function outcome) arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing new call history entries.
;;;

;;; Returns two values: the outcome to perform, and some new call history entries,
;;; or NIL if none should be added (e.g. due to eql specialization or another thread
;;; beating us to it.)
;;; This function has no side effects. DISPATCH-MISS is in charge of that.
(defun miss-info (generic-function call-history arguments)
  (let ((argument-classes (mapcar #'class-of arguments)))
    (multiple-value-bind (class-method-list ok)
        (compute-applicable-methods-using-classes generic-function argument-classes)
      (let* ((method-list (if ok
                              class-method-list
                              (compute-applicable-methods
                               generic-function arguments)))
             (method-combination
               (generic-function-method-combination generic-function))
             (final-methods (final-methods method-list argument-classes))
             (outcome (outcome
                       generic-function call-history method-combination
                       final-methods argument-classes)))
        (values
         outcome
         ;; Can we memoize the call, i.e. add it to the call history?
         (cond ((null final-methods) ; we avoid memoizing no-applicable-methods,
                ;; as it's probably just a mistake, and will just pollute the call history.
                ;; This assumption would be wrong if an application frequently called a gf
                ;; wrong and relied on the signal behavior etc,
                ;; but I find that possibility unlikely.
                nil)
               (ok ; classes are fine; use normal fastgf
                (let* ((key-length
                         (length (generic-function-specializer-profile
                                  generic-function)))
                       (key
                         (concatenate 'vector
                                      (subseq argument-classes 0 key-length))
                         ;; broken because deftype breaks on atomic specs
                         #+(or)(coerce (subseq argument-classes 0 key-length) 'vector)))
                  (if (call-history-find-key call-history key)
                      ;; another thread has already added this entry
                      nil
                      (list (cons key outcome)))))
               ((eq (class-of generic-function)
                    #.(find-class 'standard-generic-function))
                (memoize-eql-specialized generic-function method-combination
                                         call-history argument-classes))
               (t
                ;; No more options: we just don't memoize.
                ;; This only occurs with eql specializers,
                ;; at least with the standard c-a-m/-u-c methods.
                nil)))))))

(defun specializer-key-match (key1 key2)
  (declare (type simple-vector key1 key2))
  ;; Specializers can be compared by EQ, and so
  (and (= (length key1) (length key2))
       (every #'eq key1 key2)))

(defun call-history-find-key (call-history key)
  (find key call-history :key #'car :test #'specializer-key-match))

(defun memoize-eql-specialized (generic-function method-combination call-history
                                argument-classes)
  ;; we have a call with eql specialized arguments.
  ;; We can still memoize this sometimes, as long as the gf is
  ;; standard so we don't need to worry about MOP.
  ;; What we need to watch out for it the following situation-
  ;; (defmethod foo ((x (eql 'x))) ...)
  ;; (foo 'y)
  ;; If we memoize this naively,
  ;; we'll put in an entry for class SYMBOL,
  ;; and then if we call (foo 'x) later,
  ;; it will go to that instead of properly missing the cache.
  ;; EQL specializers play merry hob hell with the assumption of
  ;; fastgf that as long as you treat all classes distinctly
  ;; there are no problems with inheritance, basically.
  ;; We deal with this by memoizing every combination of eql
  ;; specializers for the given classes at once.
  (loop for spec across (generic-function-specializer-profile generic-function)
        for argument-class in argument-classes
        collect (list* argument-class
                       (if (consp spec) ; eql specialized
                           (loop for obj in spec
                                 when (typep obj argument-class)
                                   collect (intern-eql-specializer obj))
                           nil))
          into combo
        finally (return
                  (loop for speclist in (specializers-combinate combo)
                        for key = (coerce speclist 'simple-vector)
                        for omethods = (compute-applicable-methods-using-specializers
                                        generic-function speclist)
                        for methods = (final-methods omethods speclist)
                        for outcome = (outcome
                                       generic-function call-history
                                       method-combination methods
                                       argument-classes)
                        for new-entry = (cons key outcome)
                        unless (call-history-find-key call-history key)
                          collect new-entry into new-entries
                        ;; This is necessary so that OUTCOME uses the cached
                        ;; outcomes we are generating as we go.
                          and do (push new-entry call-history)
                        finally (return new-entries)))))

;;; Given a list of lists of specializers, expand out all combinations.
;;; So for example, ((a b) (c) (d e)) => ((a c d) (b c d) (a c e) (b c e))
;;; in some arbitrary order.
(defun specializers-combinate (list)
  (if (null list)
      '(nil)
      (loop with next = (specializers-combinate (rest list))
            for elem in (first list)
            nconc (loop for rest in next collect (cons elem rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing final methods.
;;;

(defun final-methods (methods specializers)
  (loop for method in methods
        collect (final-method method specializers)))

(defun final-method (method specializers)
  (let ((mc (class-of method)))
    (cond ((and
            (eq mc (load-time-value (find-class 'standard-reader-method)))
            (let ((eslotd (effective-slotd-from-accessor-method
                           method (first specializers))))
              (and (standard-slotd-p eslotd)
                   (intern-effective-reader
                    method (slot-definition-location eslotd))))))
          ((and
            (eq mc (load-time-value (find-class 'standard-writer-method)))
            (let ((eslotd (effective-slotd-from-accessor-method
                           method (second specializers))))
              (and (standard-slotd-p eslotd)
                   (intern-effective-writer
                    method (slot-definition-location eslotd))))))
          (t method))))

(defun standard-slotd-p (slotd)
  (eq (class-of slotd)
      (load-time-value (find-class 'standard-effective-slot-definition))))

(defun effective-slotd-from-accessor-method (method class)
  (let* ((direct-slot (accessor-method-slot-definition method))
         (direct-slot-name (slot-definition-name direct-slot))
         (effective-slot-defs (class-slots class))
         (slot (loop for effective-slot in effective-slot-defs
                     when (eq direct-slot-name (slot-definition-name effective-slot))
                       return effective-slot)))
    (when (null slot)
      ;; should be impossible. one way I hit it: abnormal slots from boot.lisp
      (error "BUG: cannot find effective slot for optimized accessor! class ~s, slot name ~s"
             class direct-slot-name))
    slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing outcomes
;;;


;;; Look for an existing outcome, or compute a new one if nothing matches.
(defun outcome
    (generic-function call-history method-combination methods actual-specializers)
  (or (find-existing-outcome call-history methods)
      (compute-outcome
       generic-function method-combination methods actual-specializers)))

;; We try to reuse effective method functions when possible.
;; This has two advantages: One, we avoid recompiling the same effective method multiple times.
;; Two, the code generator can understand the outcomes as identical and merge tests together.
;; Note that this being correct relies on an important property: that compute-effective-method
;; can in fact be memoized. This would not be the case if for example a method on it returns
;; different things for the same (by EQUAL) applicable method lists randomly or by time, or if
;; a relevant compute-effective-method method is added after a generic function already has
;; computed some. Or if a method combination does something similarly weird.
;; I'm not really worried about this because nobody defines methods on c-e-m anyway.
;; Also, this is a max O(mn) search, where m is the number of methods and n the length of call
;; history. It could be more efficient, but that makes it more involved to remove old entries
;; (with this scheme they're just removed with the call history entries).
(defun find-existing-outcome (call-history methods)
  (loop for (ignore . outcome) in call-history
        when (equal methods (outcome-methods outcome))
          return outcome))

(defun compute-outcome
    (generic-function method-combination methods actual-specializers)
  ;; Calculate the effective-method-function as well as an optimized one
  ;; so that we can apply the e-m-f to the arguments if we need to debug the optimized version.
  ;; This will hopefully be expanded, but for now, we can at least optimize standard slot accesses.
  ;; For that, we must determine whether there is not a custom slot-value-using-class method we have to
  ;; call. We use an approximation: if the class is a standard-class and the slotd is a
  ;; standard-effective-slot-definition, methods on svuc can't be defined per
  ;; "restrictions on portable programs" in MOP. We also discount the possibility of specializing on the
  ;; "object" argument, because it makes things harder for us with not much gain for users.
  ;; (Just specialize accessors or something.)
  ;; The upshot of this is that slot accesses will never be inlined for custom metaclasses or slotds.
  ;; The less approximate way would be to check s-v-u-c itself. That's easy enough on its own,
  ;; but also implies that methods added or removed to s-v-u-c invalidate all relevant accessors,
  ;; which is not.
  (when (null methods)
    ;; no-applicable-method is different from the no-required-method we'd get if we went below,
    ;; so we pick that off first.
    ;; Similarly to nrm below, we return a sort of fake emf.
    (return-from compute-outcome
      (make-effective-method-outcome
       :methods nil
       :form '(em-apply #'no-applicable-method .generic-function.)
       :function (lambda (&rest args)
                   (apply #'no-applicable-method generic-function args)))))
  (let* ((em (compute-effective-method generic-function method-combination methods))
         ;; will be NIL unless em = (call-method METHOD ()) or (call-method METHOD)
         (method (and (consp em)
                      (eq (first em) 'call-method)
                      (consp (cdr em))
                      (or (null (cddr em))
                          (and (consp (cddr em))
                               (null (cdddr em))
                               (null (third em))))
                      (second em))))
    (cond ((eq (class-of method)
               (load-time-value (find-class 'effective-reader-method)))
           (let ((slotd (accessor-method-slot-definition method))
                 (location
                   (with-early-accessors (effective-accessor-method)
                     (effective-accessor-method-location method)))
                 (class (first actual-specializers)))
             (make-optimized-slot-reader :index location :methods methods
                                         :slot-name (slot-definition-name slotd)
                                         :class class)))
          ((eq (class-of method)
               (load-time-value (find-class 'effective-writer-method)))
           (let ((slotd (accessor-method-slot-definition method))
                 (location
                   (with-early-accessors (effective-accessor-method)
                     (effective-accessor-method-location method)))
                 (class (second actual-specializers)))
             (make-optimized-slot-writer :index location :methods methods
                                         :slot-name (slot-definition-name slotd)
                                         :class class)))
          ;; NOTE: This case is not required if we always use :form and don't use the
          ;; interpreter. See also, comment in define-method-combination.lisp.
          ((and (consp em) (eq (first em) '%magic-no-required-method))
           (let ((group-name (second em)))
             (make-effective-method-outcome
              :methods methods :form em
              :function (lambda (&rest args)
                          (apply #'no-required-method
                                 generic-function group-name args)))))
          (t
           (make-effective-method-outcome
            :methods methods :form em
            :function (effective-method-function
                       em (gf-arg-info generic-function)))))))

;;; Not usually actually used due to compute-outcome looking for it above.
(defmacro %magic-no-required-method (group-name)
  `(em-apply #'no-required-method .generic-function. ',group-name))

(defun no-required-method (gf group-name &rest args)
  (error "No applicable methods in required group ~a for generic function ~a~@
          Given arguments: ~a"
         group-name (generic-function-name gf) args))

;;; This is used by effective-method-function
;;; to squeeze out a bit more performance by avoiding &va-rest when possible,
;;; which in turn allows methods to be called without APPLY.
(defun gf-arg-info (gf)
  (multiple-value-bind (nreq max) (generic-function-min-max-args gf)
    (append
     ;; TODO: Would be kind of nice to get something like variable names.
     (loop repeat nreq collect (gensym "REQ-ARG"))
     (list (if (or (not max) (> max nreq))
               (gensym "REST")
               nil)))))








