;;; ------------------------------------------------------------
;;;
;;; Generic function dispatch runtime
;;; This implements the algorithm described by Robert Strandh
;;;  for fast generic function dispatch. See discriminate.lisp
;;;  for the code generator, dtree.lisp for the interpreted
;;;  version, and funcallableInstance.cc for the interpreter
;;;  itself.

(in-package :clos)

;;; ------------------------------------------------------------
;;;
;;; Debugging code
;;;
;;; Add :DEBUG-FASTGF to log fastgf messages during the slow path.
;;;
#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :debug-fastgf *features*))

#+debug-fastgf
(eval-when (:execute :load-toplevel)
  (defstruct (debug-fastgf-struct (:type vector))
    stream
    didx
    indent
    miss-count)
  (defvar *dmspaces* "| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | ")
;;; Cleanup up the directory

  (defvar *dispatch-history-dir*
    (let ((dir (core:monitor-directory)))
      (ensure-directories-exist dir)
      (core:fmt *error-output* "!!!!  Created gf dispatch monitor directory: {}%N" dir)
      (core:fmt *error-output* "!!!!     Run clasp with --feature fastgf-dump-module to write dispatchers to this directory%N")
      (dolist (f (directory (core:fmt nil "{}/*.*" dir)))
        (delete-file f))
      dir))
  (defun lazy-initialize-debug-fastgf ()
    (unless core:*debug-fastgf*
      (let ((filename (core:fmt nil "{}/debug-miss-thread{}.log"
                                    *dispatch-history-dir*
                                    (mp:thread-id mp:*current-process*))))
        (setf core:*debug-fastgf* (make-debug-fastgf-struct :stream (open filename :direction :output)
                                                            :didx 0
                                                            :indent 0
                                                            :miss-count (make-hash-table))))))
  (defun debug-fastgf-stream ()
    (lazy-initialize-debug-fastgf)
    (debug-fastgf-struct-stream core:*debug-fastgf*))
 
  (defun debug-fastgf-didx ()
    (lazy-initialize-debug-fastgf)
    (debug-fastgf-struct-didx core:*debug-fastgf*))
  (defun incf-debug-fastgf-didx ()
    (incf (debug-fastgf-struct-didx core:*debug-fastgf*)))
  
  (defun debug-fastgf-miss-count (gf)
    (gethash gf (debug-fastgf-struct-miss-count core:*debug-fastgf*) 0))
  (defun incf-debug-fastgf-miss-count (gf)
    (lazy-initialize-debug-fastgf)
    (incf (gethash gf (debug-fastgf-struct-miss-count core:*debug-fastgf*) 0)))
    
  (defun debug-fastgf-indent ()
    (debug-fastgf-struct-indent core:*debug-fastgf*))
  (defun incf-debug-fastgf-indent ()
    (lazy-initialize-debug-fastgf)
    (incf (debug-fastgf-struct-indent core:*debug-fastgf*) 2))
  (defun decf-debug-fastgf-indent ()
    (decf (debug-fastgf-struct-indent core:*debug-fastgf*) 2))
  (defmacro fmt-noindent (fmt &rest args)
    `(progn
       (lazy-initialize-debug-fastgf)
       (core:fmt (debug-fastgf-stream) ,fmt ,@args)))
  (defmacro fmt-indent (fmt &rest args)
    `(progn
       (lazy-initialize-debug-fastgf)
       (core:fmt (debug-fastgf-stream) (subseq *dmspaces* 0 (min (length *dmspaces*) (debug-fastgf-indent))))
       (core:fmt (debug-fastgf-stream) ,fmt ,@args)))
  (defun graph-call-history (generic-function output)
    (generate-dot-file generic-function output))
  (defun log-cmpgf-filename (gfname suffix extension)
    (pathname (core:fmt nil "{}/dispatch-thread{}-{:0>5d}-{}.{}"
                            *dispatch-history-dir*
                            (mp:thread-id mp:*current-process*)
                            suffix
                            (debug-fastgf-didx)
                            (core:tostring gfname)
                            extension)))
  (defmacro gf-log-dispatch-graph (gf)
    `(graph-call-history ,gf (log-cmpgf-filename (clos::generic-function-name gf) "graph" "dot")))
  (defmacro gf-log-dispatch-miss-followup (msg &rest args)
    `(progn
       (fmt-indent "------- ")
       (fmt-noindent ,msg ,@args)))
  (defmacro gf-log-dispatch-miss-message (msg &rest args)
    `(fmt-indent ,msg ,@args))
  (defmacro gf-log-sorted-roots (roots)
    `(progn
       (fmt-indent ">>> sorted roots%N")
       (let ((x 0))
         (mapc (lambda (root)
                 (fmt-indent "  root[{}]: {}%N" (prog1 x (incf x)) root))))))
  (defun pretty-selector-as-string (selector)
    (cond
      ((eql-specializer-p selector)
       (with-early-accessors (+eql-specializer-slots+)
         (core:fmt nil "(EQL {})" (eql-specializer-object selector))))
      ((null selector)
       (core:fmt nil "NULL/(not-specialized?)"))
      ((classp selector)               ; A class
       (core:fmt nil "[class {}/{}]" (class-name selector) (core:class-stamp-for-instances selector)))
      (t                                ; This shouldn't happen
       (core:fmt nil "!!!!!ILLEGAL-SELECTOR-IN-CALL-HISTORY-ENTRY-KEY!!!!!"))))
  (defmacro gf-print-entry (index entry)
    (let ((selector (gensym))
          (key (gensym)))
      `(progn
         (fmt-indent "        entry#{:3d}: (" (prog1 ,index (incf ,index)))
         (let ((,key (car ,entry)))
           ;;(fmt-indent "          ----> {}%N" history-entry)
           (dolist (,selector (coerce ,key 'list))
             (fmt-noindent " {}" (pretty-selector-as-string ,selector)))
           (fmt-noindent ")%N")))))
  (defun %gf-log-dispatch-miss (msg gf args)
    (incf-debug-fastgf-didx)
    (incf-debug-fastgf-miss-count gf)
    (fmt-indent "------- DIDX:{} {}%N" (debug-fastgf-didx) msg)
    (fmt-indent "Dispatch miss #{} for {}%N" (debug-fastgf-miss-count gf)
                    (generic-function-name gf))
       (let* ((call-history (mp:atomic (safe-gf-call-history gf)))
              (specializer-profile (safe-gf-specializer-profile gf)))
         (fmt-indent "    args (num args -> {}):  %N" (length args))
         (let ((arg-index -1))
           (dolist (arg args)
             (fmt-indent "argument# {}: {}[{}/{}] %N"
                             (incf arg-index) arg (class-of arg)
                             (core:instance-stamp arg))))
         (let ((index 0))
           (fmt-indent " raw call-history (length -> {}):%N" (length call-history))
           (dolist (entry call-history)
             (gf-print-entry index entry)))
         (let* ((call-history (mp:atomic (safe-gf-call-history gf)))
                (specializer-profile (safe-gf-specializer-profile gf))
                (index 0))
           (fmt-indent "    call-history (length -> {}):%N" (length call-history))
           (dolist (entry call-history)
             (gf-print-entry index entry))))
       (finish-output (debug-fastgf-stream)))
  (defmacro gf-log-dispatch-miss (msg gf args)
    `(%gf-log-dispatch-miss ,msg ,gf ,args))
  (defmacro gf-log (fmt &rest fmt-args) `(fmt-indent ,fmt ,@fmt-args))
  (defmacro gf-log-noindent (fmt &rest fmt-args) `(fmt-noindent ,fmt ,@fmt-args))
  (defmacro gf-do (&body code) `(progn ,@code)))

#-debug-fastgf
(eval-when (:execute :load-toplevel)
  (defmacro gf-log-sorted-roots (roots) (declare (ignore roots)))
  (defmacro gf-log-dispatch-graph (gf) (declare (ignore gf)))
  (defmacro gf-log-dispatch-miss (msg gf args)
    (declare (ignore msg gf args)))
  (defmacro gf-log-dispatch-miss-followup (msg &rest args)
    (declare (ignore msg args)))
  (defmacro gf-log-dispatch-miss-message (msg &rest args)
    (declare (ignore msg args)))
  (defmacro gf-log (fmt &rest fmt-args) (declare (ignore fmt fmt-args)))
  (defmacro gf-log-noindent (fmt &rest fmt-args)
    (declare (ignore fmt fmt-args)))
  (defmacro gf-do (&body code) (declare (ignore code)))
  (defun incf-debug-fastgf-indent ())
  (defun decf-debug-fastgf-indent ())
  )

;;; --------------------------------------------------
;;;
;;; This section contains code that is called by CLOS to
;;;   update generic-function-call-history and to call
;;;   codegen-dispatcher to generate a new dispatch function when needed
;;;

;; Returns true iff the instance was updated.
(defun maybe-update-instance (instance)
  (when (and (core:instancep instance)
             (si:sl-boundp (si:instance-sig instance)))
    (with-early-accessors (+standard-class-slots+)
      (let ((instance-stamp (core:instance-stamp instance))
            (class-stamp (core:class-stamp-for-instances
                          (core:instance-class instance))))
        (unless (= instance-stamp class-stamp)
          (gf-log "   instance-stamp matches that of class -> {}%N"
                  (= instance-stamp class-stamp))
          (gf-log "(core:instance-stamp i) -> {}%N" instance-stamp)
          (gf-log "(core:class-stamp-for-instances (core:instance-class i)) -> {}%N"
                  class-stamp)
          (update-instance instance)
          t)))))

(defun maybe-update-instances (arguments)
  (let ((invalid-instance nil))
    (dolist (i arguments invalid-instance)
      (setf invalid-instance (or (maybe-update-instance i) invalid-instance)))))

(defun applicable-method-p (method specializers)
  (loop for spec in (method-specializers method)
        for argspec in specializers
        always (cond ((eql-specializer-p spec)
                      (and (eql-specializer-p argspec)
                           (eql (eql-specializer-object argspec)
                                (eql-specializer-object spec))))
                     ((eql-specializer-p argspec)
                      ;; This is (typep (e-s-o ...) spec) but we know spec is
                      ;; a class so we skip to this.
                      (si:subclassp (class-of (eql-specializer-object argspec))
                                 spec))
                     (t (si:subclassp argspec spec)))))

;;; This "fuzzed" applicable-method-p is used in
;;; update-call-history-for-add-method, below, to handle added EQL-specialized
;;; methods properly. See bug #1009.
(defun fuzzed-applicable-method-p (method specializers)
  (loop for spec in (method-specializers method)
        for argspec in specializers
        always (cond ((eql-specializer-p spec)
                      (if (eql-specializer-p argspec)
                          (eql (eql-specializer-object argspec)
                               (eql-specializer-object spec))
                          (si:subclassp argspec
                                     (class-of (eql-specializer-object spec)))))
                     ((eql-specializer-p argspec)
                      (si:subclassp (class-of (eql-specializer-object argspec))
                                 spec))
                     (t (si:subclassp argspec spec)))))

(defun applicable-method-list-using-specializers (gf specializers)
  (declare (optimize (speed 3)))
  (with-early-accessors (+standard-method-slots+
			 +standard-generic-function-slots+
			 +eql-specializer-slots+
			 +standard-class-slots+)
    (loop for method in (generic-function-methods gf)
       when (applicable-method-p method specializers)
       collect (maybe-replace-method method specializers))))

(defun compute-applicable-methods-using-specializers (generic-function specializers)
  (check-type specializers list)
  (sort-applicable-methods
   generic-function
   (applicable-method-list-using-specializers generic-function specializers)
   specializers))

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

(defun optimizable-reader-method-p (method)
  ;; In the future, we could use load-time-value instead of find-class every time,
  ;; but the system loading architecture makes this dicey at the moment.
  (eq (class-of method) (find-class 'standard-reader-method)))

(defun optimizable-writer-method-p (method)
  (eq (class-of method) (find-class 'standard-writer-method)))

(defun standard-slotd-p (slotd)
  (eq (class-of slotd) (find-class 'standard-effective-slot-definition)))

(defun maybe-replace-method (method specializers)
  (let ((mc (class-of method)))
    (cond ((and
            (eq mc (find-class 'standard-reader-method))
            (let ((eslotd (effective-slotd-from-accessor-method
                           method (first specializers))))
              (and (standard-slotd-p eslotd)
                   (intern-effective-reader
                    method (slot-definition-location eslotd))))))
          ((and
            (eq mc (find-class 'standard-writer-method))
            (let ((eslotd (effective-slotd-from-accessor-method
                           method (second specializers))))
              (and (standard-slotd-p eslotd)
                   (intern-effective-writer
                    method (slot-definition-location eslotd))))))
          (t method))))

(defun final-methods (methods specializers)
  (loop for method in methods
        collect (maybe-replace-method method specializers)))

;;; the gf-arg-info of a generic-function is a cons (boolean . vars)
;;; where vars is a list of symbols. This is used by effective-method-function
;;; to squeeze out a bit more performance by avoiding &va-rest when possible,
;;; which in turn allows methods to be called without APPLY.
;;; the boolean is whether a &rest is needed (so, whether there's an &optional,
;;; &rest, or &key in the generic function lambda list) and the vars are
;;; suggestions for the required parameters. The length has to be correct.
;;; so e.g. (T a b c) means a lambda list of (a b c &rest more) or so.
(defun gf-arg-info (gf)
  (multiple-value-bind (nreq max) (generic-function-min-max-args gf)
    (cons (or (not max) (> max nreq))
          ;; TODO: Would be kind of nice to get something like variable names.
          (loop repeat nreq collect (gensym "REQ-ARG")))))

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
       :function (lambda (core:&va-rest vaslist-args)
                   (apply #'no-applicable-method generic-function vaslist-args)))))
  (let* ((em (compute-effective-method generic-function method-combination methods))
         ;; will be NIL unless em = (call-method METHOD ()) or (call-method METHOD)
         (method (and (consp em)
                      (eq (first em) 'call-method)
                      (consp (cdr em))
                      (or (null (cddr em))
                          (and (consp (cddr em))
                               (null (cdddr em))
                               (null (third em))))
                      (second em)))
         (optimized
           (cond ((eq (class-of method) (find-class 'effective-reader-method))
                  (let ((slotd (accessor-method-slot-definition method))
                        (location
                          (with-early-accessors (+effective-accessor-method-slots+)
                            (effective-accessor-method-location method)))
                        (class (first actual-specializers)))
                    (make-optimized-slot-reader :index location :methods methods
                                                :slot-name (slot-definition-name slotd)
                                                :class class)))
                 ((eq (class-of method) (find-class 'effective-writer-method))
                  (let ((slotd (accessor-method-slot-definition method))
                        (location
                          (with-early-accessors (+effective-accessor-method-slots+)
                            (effective-accessor-method-location method)))
                        (class (second actual-specializers)))
                    (make-optimized-slot-writer :index location :methods methods
                                                :slot-name (slot-definition-name slotd)
                                                :class class)))
                 ;; NOTE: This case is not required if we always use :form and don't use the
                 ;; interpreter. See also, comment in combin.lisp.
                 ((and (consp em) (eq (first em) '%magic-no-required-method))
                  (gf-log "No-required-method as effective method%N")
                  (gf-log "em: {}%N" em)
                  (let ((group-name (second em)))
                    (make-effective-method-outcome
                     :methods methods :form em
                     :function (lambda (core:&va-rest vaslist-args)
                                 (apply #'no-required-method
                                        generic-function group-name vaslist-args)))))
                 (t
                  (gf-log "Using default effective method function%N")
                  (gf-log "(compute-effective-method generic-function method-combination methods) -> %N")
                  (gf-log "{}%N" em)
                  (make-effective-method-outcome
                   :methods methods :form em
                   :function (effective-method-function
                              em (gf-arg-info generic-function)))))))
    #+debug-fastgf
    (progn
      (gf-log "vvv************************vvv%N")
      (gf-log "compute-effective-method-function for {}%N" (generic-function-name generic-function))
      (gf-log "There are {} methods...%N" (length methods))
      (dolist (m methods)
        (gf-log "Method: {} {} {}%N" (clos::method-specializers m) (clos::method-qualifiers m) m))
      (gf-log "Effective method function -> {}%N" optimized)
      (gf-log "^^^************************^^^%N"))
    optimized))

(defun outcome
    (generic-function call-history method-combination methods actual-specializers)
  (or (find-existing-outcome call-history methods)
      (compute-outcome
       generic-function method-combination methods actual-specializers)))

(defun update-call-history-for-add-method (call-history method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to."
  (loop for entry in call-history
        for specializers = (coerce (car entry) 'list)
        unless (fuzzed-applicable-method-p method specializers)
          collect entry))

(defun update-generic-function-call-history-for-add-method (generic-function method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (mp:atomic-update (safe-gf-call-history generic-function)
                    #'update-call-history-for-add-method
                    method))

(defun update-call-history-for-remove-method (call-history method)
  (let (new-call-history)
    (loop for entry in call-history
          for specializers = (coerce (car entry) 'list)
          unless (applicable-method-p method specializers)
            do (push (cons (car entry) (cdr entry)) new-call-history))
    new-call-history))

(defun update-generic-function-call-history-for-remove-method (generic-function method)
  "When a method is removed then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to
    AND if that means there are no methods left that apply to the specializers
     then remove the entry from the list.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (mp:atomic-update (safe-gf-call-history generic-function)
                    #'update-call-history-for-remove-method
                    method))

;;; FIXME: Replace with atomic setf
(defun erase-generic-function-call-history (generic-function)
  (setf (mp:atomic (safe-gf-call-history generic-function)) nil))

(defun specializer-key-match (key1 key2)
  (declare (type simple-vector key1 key2))
  ;; Specializers can be compared by EQ, and so
  (and (= (length key1) (length key2))
       (every #'eq key1 key2)))

(defun call-history-find-key (call-history memoized-key)
  "Return true if the given key is already present in the history, or else nil."
  (loop for (key . ignore) in call-history
        when (specializer-key-match key memoized-key) do (return-from call-history-find-key t))
  nil)

(defun specializer-call-history-generic-functions-push-new (specializer generic-function)
  (with-early-accessors (+specializer-slots+)
    (mp:with-rwlock ((specializer-mutex specializer) :write)
      (pushnew generic-function (specializer-call-history-generic-functions specializer)
               :test #'eq))))

(defun check-long-call-history (generic-function)
  #-debug-long-call-history
  (declare (ignore generic-function))
  #+debug-long-call-history
  (when (> (length (generic-function-call-history generic-function)) 16384)
    (error "DEBUG-LONG-CALL-HISTORY is triggered - The call history for ~a is longer (~a entries) than 16384" generic-function (length (generic-function-call-history generic-function)))))

(defun schgf-pushnew (memoized-key generic-function)
  (loop for specializer across (the simple-vector memoized-key)
        unless (eql-specializer-p specializer)
          do (specializer-call-history-generic-functions-push-new
              specializer generic-function)))

(defun perform-outcome (outcome arguments)
  (cond
    ((optimized-slot-reader-p outcome)
     ;; Call is like (name instance)
     (let ((value (standard-location-access
                   (first arguments) (optimized-slot-reader-index outcome))))
       (if (si:sl-boundp value)
           value
           (values (slot-unbound (optimized-slot-reader-class outcome) (first arguments)
                                 (optimized-slot-reader-slot-name outcome))))))
    ((optimized-slot-writer-p outcome)
     ;; Call is like ((setf name) new-value instance)
     (setf (standard-location-access
            (second arguments) (optimized-slot-writer-index outcome))
           (first arguments)))
    ((effective-method-outcome-p outcome)
     (let ((function (effective-method-outcome-function outcome)))
       (assert (not (null function))) ; FIXME: REMOVE
       (apply function arguments)))
    (t (error "BUG: Bad thing to be an outcome: ~a" outcome))))

#+debug-fastgf
(defvar *dispatch-miss-start-time*)

;;; Given a list of lists of specializers, expand out all combinations.
;;; So for example, ((a b) (c) (d e)) => ((a c d) (b c d) (a c e) (b c e))
;;; in some arbitrary order.
(defun specializers-combinate (list)
  (if (null list)
      '(nil)
      (loop with next = (specializers-combinate (rest list))
            for elem in (first list)
            nconc (loop for rest in next collect (cons elem rest)))))

;;; Returns two values: the outcome to perform, and some new call history entries,
;;; or NIL if none should be added (e.g. due to eql specialization or another thread
;;; beating us to it.)
;;; This function has no side effects. DISPATCH-MISS is in charge of that.
(defun dispatch-miss-info (generic-function call-history arguments)
  (let ((argument-classes (mapcar #'class-of arguments)))
    (multiple-value-bind (class-method-list ok)
        (compute-applicable-methods-using-classes generic-function argument-classes)
      (gf-log "Called compute-applicable-methods-using-classes - returned method-list: {}  ok: {}%N"
              class-method-list ok)
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
                (gf-log-dispatch-miss "No applicable method"
                                      generic-function arguments)
                nil)
               (ok ; classes are fine; use normal fastgf
                (gf-log-dispatch-miss "Memoizing normal call"
                                      generic-function arguments)
                (let* ((key-length
                         (length (safe-gf-specializer-profile generic-function)))
                       (key (coerce (subseq argument-classes 0 key-length) 'vector)))
                  (if (find key call-history :key #'car :test #'specializer-key-match)
                      ;; another thread has already added this entry
                      nil
                      (list (cons key outcome)))))
               ((eq (class-of generic-function)
                    (find-class 'standard-generic-function))
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
                (gf-log-dispatch-miss "Memoizing eql-specialized call"
                                      generic-function arguments)
                (loop for spec across (safe-gf-specializer-profile
                                       generic-function)
                      for argument-class in argument-classes
                      collect (list*
                               argument-class
                               (if (consp spec) ; eql specialized
                                   (loop for obj in spec
                                         when (typep obj argument-class)
                                           collect (intern-eql-specializer obj))
                                   nil))
                        into combo
                      finally (let ((speclists (specializers-combinate combo)))
                                (return
                                  (loop for speclist in speclists
                                        for key = (coerce speclist 'simple-vector)
                                        for methods = (compute-applicable-methods-using-specializers generic-function speclist)
                                        for outcome = (outcome
                                                       generic-function
                                                       call-history
                                                       method-combination
                                                       methods
                                                       argument-classes)
                                        for new-entry = (cons key outcome)
                                        unless (find key call-history :key #'car
                                                     :test #'specializer-key-match)
                                          collect new-entry into new-entries
                                          ;; This is necessary so that OUTCOME uses the cached
                                          ;; outcomes we are generating as we go.
                                          and do (push new-entry call-history)
                                        finally (return new-entries))))))
               (t
                ;; No more options: we just don't memoize.
                ;; This only occurs with eql specializers,
                ;; at least with the standard c-a-m/-u-c methods.
                (gf-log-dispatch-miss "Cannot memoize call" generic-function arguments)
                nil)))))))

(defun check-gf-argcount (generic-function nargs)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (when (or (< nargs min) (and max (> nargs max)))
      (error 'core:wrong-number-of-arguments
             :called-function generic-function :given-nargs nargs
             :min-nargs min :max-nargs max))))

(defun union-entries (old-call-history new-entries)
  ;; We do this instead of UNION because the new entries can contain duplicates.
  (loop for entry in new-entries do (pushnew entry old-call-history
                                             :key #'car :test #'specializer-key-match))
  old-call-history)

(defun memoize-entries (old-call-history generic-function arguments)
  (union-entries old-call-history
                 (dispatch-miss-info generic-function old-call-history arguments)))

(defun dispatch-miss (generic-function &rest arguments)
  (#+debug-fastgf unwind-protect #-debug-fastgf multiple-value-prog1
   (progn
     (incf-debug-fastgf-indent)
     (check-gf-argcount generic-function (length arguments))
     ;; Update any invalid instances
     (when (maybe-update-instances arguments)
       (return-from dispatch-miss (apply generic-function arguments)))
     ;; OK, real miss.
     #+debug-fastgf
     (progn
       (gf-log "----{---- A dispatch-miss occurred[(1- (core:next-number))->{}]  -> {}  %N" (1- (core:next-number)) (clos::generic-function-name generic-function))
       (dolist (arg arguments)
         (gf-log "{}[{}/{}] " (core:safe-repr arg) (core:safe-repr (class-of arg)) (core:instance-stamp arg)))
       (gf-log-noindent "%N"))
     (let (#+debug-fastgf
           (*dispatch-miss-start-time* (get-internal-real-time))
           ;; We have to recompute the new entries in the CAS loop because we need to
           ;; ensure that outcome= works, i.e. that we don't end up with two distinct outcome
           ;; objects in the call history that represent the same effective method. This would
           ;; screw up discriminator generation; see #
           outcome updatedp)
       (mp:atomic-update (safe-gf-call-history generic-function)
                         (lambda (call-history)
                           (multiple-value-bind (noutcome new-entries)
                               (dispatch-miss-info generic-function call-history arguments)
                             (setf outcome noutcome)
                             (cond ((null new-entries)
                                    (setf updatedp nil)
                                    call-history)
                                   (t (setf updatedp t)
                                      (union-entries call-history new-entries))))))
       (when updatedp (force-dispatcher generic-function))
       (gf-log "Performing outcome {}%N" outcome)
       #+debug-fastgf
       (let ((results (multiple-value-list
                       (perform-outcome outcome arguments))))
         (gf-log "+-+-+-+-+-+-+-+-+ dispatch-miss done real time: %f seconds%N" (/ (float (- (get-internal-real-time) *dispatch-miss-start-time*)) internal-time-units-per-second))
         (gf-log "----}---- Completed call to effective-method-function for {} results -> {}%N" (clos::generic-function-name generic-function) results)
         (values-list results))
       #-debug-fastgf
       (perform-outcome outcome arguments)))
   (decf-debug-fastgf-indent)))

;;; Called from the dtree interpreter,
;;; because APPLY from C++ is kind of annoying.
(defun dispatch-miss-va (generic-function vaslist-args)
  (apply #'dispatch-miss generic-function vaslist-args))

(defvar *fastgf-force-compiler* nil)
(defun calculate-fastgf-dispatch-function
    (generic-function &key (compile *fastgf-force-compiler*))
  (if (mp:atomic (safe-gf-call-history generic-function))
      (let ((timer-start (get-internal-real-time)))
        (unwind-protect
             (if (and #-cclasp nil compile cmp:*cleavir-compile-hook*)
                 (compile nil (generate-discriminator generic-function))
                 (bytecode-interpreted-discriminator generic-function))
          (let ((delta-seconds (/ (float (- (get-internal-real-time) timer-start) 1d0)
                                  internal-time-units-per-second)))
            (gctools:accumulate-discriminating-function-compilation-seconds delta-seconds))))
      (invalidated-discriminating-function-closure generic-function)))

(defun force-dispatcher (generic-function)
  (let (log-output)
    #-debug-fastgf (declare (ignore log-output))
    #+debug-fastgf
    (progn
      (if (eq (class-of generic-function) (find-class 'standard-generic-function))
          (let ((generic-function-name (core:low-level-standard-generic-function-name generic-function)))
            (setf log-output (log-cmpgf-filename generic-function-name "func" "ll"))
            (gf-log "Writing dispatcher to {}%N" log-output))
          (setf log-output (log-cmpgf-filename (generic-function-name generic-function) "func" "ll")))
      (incf-debug-fastgf-didx))
    (set-funcallable-instance-function generic-function
                                       (calculate-fastgf-dispatch-function
                                        generic-function))))

;;; Used by interpret-dtree-program.
(defun compile-discriminating-function (generic-function)
  ;; Ensure an up to date (i.e. won't need to miss on these arguments)
  ;; interpreted discriminator is installed, so that if Cleavir calls the
  ;; generic function we're compiling it won't go recursive.
  (set-funcallable-instance-function generic-function
                                     (calculate-fastgf-dispatch-function
                                      generic-function :compile nil))
  (set-funcallable-instance-function generic-function
                                     (calculate-fastgf-dispatch-function
                                      generic-function :compile t)))

#+debug-fastgf
(defvar *dispatch-miss-recursion-check* nil)

(defun invalidated-dispatch-function (generic-function vaslist-args)
  #+(or)(declare (optimize (debug 3)))
  #+debug-fastgf
  (when (find (cons generic-function (core:list-from-vaslist vaslist-args)) *dispatch-miss-recursion-check*
              :test #'equal)
    (format t "~&Recursive dispatch miss detected~%")
    (ext:quit 1))
  (let (#+debug-fastgf
        (*dispatch-miss-recursion-check* (cons (cons generic-function
                                                     (core:list-from-vaslist vaslist-args))
                                               *dispatch-miss-recursion-check*)))

  ;;; If there is a call history then compile a dispatch function
  ;;;   being extremely careful NOT to use any generic-function calls.
  ;;;   Then redo the call.
  ;;; If there is no call history then treat this like a dispatch-miss.
  #+debug-fastgf
  (if (eq (class-of generic-function) (find-class 'standard-generic-function))
      (gf-log "Entered invalidated-dispatch-function for {} - avoiding generic function calls until return!!!%N"
              (core:low-level-standard-generic-function-name generic-function))
      (gf-log "Entered invalidated-dispatch-function - avoiding generic function calls until return!!!%N"))
  (gf-log "Specializer profile is {}%N" (safe-gf-specializer-profile generic-function))
  (if (mp:atomic (safe-gf-call-history generic-function))
      (progn
        (force-dispatcher generic-function)
        (apply generic-function vaslist-args))
      (apply #'dispatch-miss generic-function vaslist-args))))

;;; I don't believe the following few functions are called from anywhere, but they may be useful for debugging.

#+(or)
(defun method-spec-matches-entry-spec (method-spec entry-spec)
  (or
   (and (consp method-spec)
        (consp entry-spec)
        (eq (car method-spec) 'eql)
        (eql (second method-spec) (car entry-spec)))
   (and (classp method-spec) (classp entry-spec)
        (member method-spec (class-precedence-list entry-spec)))))

#+(or)
(defun call-history-entry-involves-method-with-specializers (entry method-specializers)
  (let ((key (car entry)))
    (loop for method-spec in method-specializers
       for entry-spec across key
       always (method-spec-matches-entry-spec method-spec entry-spec))))

#+(or)
(defun call-history-after-method-with-specializers-change (gf method-specializers)
  (loop for entry in (mp:atomic (safe-gf-call-history gf))
     unless (call-history-entry-involves-method-with-specializers entry method-specializers)
       collect entry))

#+(or)
(defun call-history-after-class-change (gf class)
;;;  (format t "call-history-after-class-change  start: gf->~a  call-history ->~a~%" gf (clos::generic-function-cal-history gf))
  (loop for entry in (mp:atomic (safe-gf-call-history gf))
     unless (loop for subclass in (subclasses* class)
               thereis (call-history-entry-key-contains-specializer (car entry) subclass))
     collect entry))

(defun subclasses* (class)
  (remove-duplicates
   (cons class
         (reduce #'append (mapcar #'subclasses*
                                  (class-direct-subclasses class))))))

(defun call-history-entry-key-contains-specializers-p (key specializer)
  (find specializer key :test #'eq))

(defun generic-function-call-history-separate-entries-with-specializer
    (call-history gf specializer)
  (declare (ignorable gf))
  (gf-log "generic-function-call-history-remove-entries-with-specializers  gf: {}%N    specializers: {}%N" gf specializers)
  (loop for entry in call-history
        for key = (car entry)
        do (gf-log "         check if entry key: {}   contains specializer: {}%N" key specializers)
        if (call-history-entry-key-contains-specializers-p key specializer)
          do (gf-log "       It does - removing entry%N")
          and collect entry into removed
        else
          do (gf-log "       It does not - keeping entry%N")
          and collect entry into keep
        finally (return (values keep removed))))

;; Remove all call entries referring directly to a class, and invalidate or
;; force their discriminating functions.
(defun invalidate-generic-functions-with-class-selector (class)
  (gf-log "invalidate-generic-functions-with-class-selector {}%N" class)
  (let ((generic-functions (specializer-call-history-generic-functions class)))
    (gf-log "   for class {} there are {} generic functions%N"
            class (length generic-functions))
    (gf-log "         generic functions -> {}%N" generic-functions)
    (loop for gf in generic-functions
          do (gf-log "generic function: {}%N" (clos:generic-function-name gf))
             (gf-log "    (clos:get-funcallable-instance-function gf) -> {}%N"
                     (clos:get-funcallable-instance-function gf))
             (let ((new-call-history
                     (mp:atomic-update (safe-gf-call-history gf)
                                       #'generic-function-call-history-separate-entries-with-specializer
                                       gf class)))
               (declare (ignorable new-call-history))
               (gf-log "    edited call history%N")
               (gf-log "{}%N" new-call-history)
               (gf-log "Invalidating discriminating function%N")
               ;; We don't force the dispatcher, because whena class with
               ;; subclasses is redefined, we may end up here repeatedly.
               ;; Eagerness would result in pointless compilation.
               (invalidate-discriminating-function gf)))))

;;; This is called by the dtree interpreter when it doesn't get enough arguments,
;;; because computing this stuff in C++ would be needlessly annoying.
(defun interp-wrong-nargs (generic-function given-nargs)
  (multiple-value-bind (min max) (generic-function-min-max-args generic-function)
    (error 'core:wrong-number-of-arguments
           :called-function generic-function :given-nargs given-nargs
           :min-nargs min :max-nargs max)))


;;; Implemented by Bike June 22, 2021

(defun maybe-compile-named-gf (name)
  (when (fboundp name)
    (let ((f (fdefinition name)))
      (when (typep f 'standard-generic-function)
        (clos:compile-discriminating-function f)))))

(defun compile-all-generic-functions ()
  (do-all-symbols (s)
    (maybe-compile-named-gf s)
    (maybe-compile-named-gf `(setf ,s))))

(export 'compile-all-generic-functions)
