;;; ------------------------------------------------------------
;;;
;;; Generic function dispatch runtime
;;; This implements the algorithm described by Robert Strandh
;;;  for fast generic function dispatch. See discriminate.lsp
;;;  for the code generator, dtree.lsp for the interpreted
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
      (core:bformat *error-output* "!!!!  Created gf dispatch monitor directory: %s%N" dir)
      (core:bformat *error-output* "!!!!     Run clasp with --feature fastgf-dump-module to write dispatchers to this directory%N")
      (dolist (f (directory (core:bformat nil "%s/*.*" dir)))
        (delete-file f))
      dir))
  (defun lazy-initialize-debug-fastgf ()
    (unless core:*debug-fastgf*
      (let ((filename (core:bformat nil "%s/debug-miss-thread%s.log"
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
  (defmacro bformat-noindent (fmt &rest args)
    `(progn
       (lazy-initialize-debug-fastgf)
       (core:bformat (debug-fastgf-stream) ,fmt ,@args)))
  (defmacro bformat-indent (fmt &rest args)
    `(progn
       (lazy-initialize-debug-fastgf)
       (core:bformat (debug-fastgf-stream) (subseq *dmspaces* 0 (min (length *dmspaces*) (debug-fastgf-indent))))
       (core:bformat (debug-fastgf-stream) ,fmt ,@args)))
  (defun graph-call-history (generic-function output)
    (generate-dot-file generic-function output))
  (defun log-cmpgf-filename (gfname suffix extension)
    (pathname (core:bformat nil "%s/dispatch-thread%s-%s%05d-%s.%s"
                            *dispatch-history-dir*
                            (mp:thread-id mp:*current-process*)
                            suffix
                            (debug-fastgf-didx)
                            (core:bformat nil "%s" gfname)
                            extension)))
  (defmacro gf-log-dispatch-graph (gf)
    `(graph-call-history ,gf (log-cmpgf-filename (clos::generic-function-name gf) "graph" "dot")))
  (defmacro gf-log-dispatch-miss-followup (msg &rest args)
    `(progn
       (bformat-indent "------- ")
       (bformat-noindent ,msg ,@args)))
  (defmacro gf-log-dispatch-miss-message (msg &rest args)
    `(bformat-indent ,msg ,@args))
  (defmacro gf-log-sorted-roots (roots)
    `(progn
       (bformat-indent ">>> sorted roots%N")
       (let ((x 0))
         (mapc (lambda (root)
                 (bformat-indent "  root[%d]: %s%N" (prog1 x (incf x)) root))))))
  (defun pretty-selector-as-string (selector)
    (cond
      ((eql-specializer-p selector)
       (with-early-accessors (+eql-specializer-slots+)
         (core:bformat nil "(EQL %s)" (eql-specializer-object selector))))
      ((null selector)
       (core:bformat nil "NULL/(not-specialized?)"))
      ((classp selector)               ; A class
       (core:bformat nil "[class %s/%d]" (class-name selector) (core:class-stamp-for-instances selector)))
      (t                                ; This shouldn't happen
       (core:bformat nil "!!!!!ILLEGAL-SELECTOR-IN-CALL-HISTORY-ENTRY-KEY!!!!!"))))
  (defmacro gf-print-entry (index entry)
    (let ((selector (gensym))
          (key (gensym)))
      `(progn
         (bformat-indent "        entry#%3d: (" (prog1 ,index (incf ,index)))
         (let ((,key (car ,entry)))
           ;;(bformat-indent "          ----> %s%N" history-entry)
           (dolist (,selector (coerce ,key 'list))
             (bformat-noindent " %s" (pretty-selector-as-string ,selector)))
           (bformat-noindent ")%N")))))
  (defun %gf-log-dispatch-miss (msg gf args)
    (incf-debug-fastgf-didx)
    (incf-debug-fastgf-miss-count gf)
    (bformat-indent "------- DIDX:%d %s%N" (debug-fastgf-didx) msg)
    (bformat-indent "Dispatch miss #%d for %s%N" (debug-fastgf-miss-count gf)
                    (generic-function-name gf))
       (let* ((call-history (safe-gf-call-history gf))
              (specializer-profile (safe-gf-specializer-profile gf)))
         (bformat-indent "    args (num args -> %d):  %N" (length args))
         (let ((arg-index -1))
           (dolist (arg args)
             (bformat-indent "argument# %d: %s[%s/%d] %N"
                             (incf arg-index) arg (class-of arg)
                             (core:instance-stamp arg))))
         (let ((index 0))
           (bformat-indent " raw call-history (length -> %d):%N" (length call-history))
           (dolist (entry call-history)
             (gf-print-entry index entry)))
         (let* ((call-history (safe-gf-call-history gf))
                (specializer-profile (safe-gf-specializer-profile gf))
                (index 0))
           (bformat-indent "    call-history (length -> %d):%N" (length call-history))
           (dolist (entry call-history)
             (gf-print-entry index entry))))
       (finish-output (debug-fastgf-stream)))
  (defmacro gf-log-dispatch-miss (msg gf args)
    `(%gf-log-dispatch-miss ,msg ,gf ,args))
  (defmacro gf-log (fmt &rest fmt-args) `(bformat-indent ,fmt ,@fmt-args))
  (defmacro gf-log-noindent (fmt &rest fmt-args) `(bformat-noindent ,fmt ,@fmt-args))
  (defmacro gf-do (&body code) `(progn ,@code)))

#-debug-fastgf
(eval-when (:execute :load-toplevel)
  (defmacro gf-log-sorted-roots (roots) nil)
  (defmacro gf-log-dispatch-graph (gf) nil)
  (defmacro gf-log-dispatch-miss (msg gf args) nil)
  (defmacro gf-log-dispatch-miss-followup (msg &rest args) nil)
  (defmacro gf-log-dispatch-miss-message (msg &rest args) nil)
  (defmacro gf-log (fmt &rest fmt-args) nil)
  (defmacro gf-log-noindent (fmt &rest fmt-args) nil)
  (defmacro gf-do (&body code) nil)
  (defun incf-debug-fastgf-indent ())
  (defun decf-debug-fastgf-indent ())
  )

;;; --------------------------------------------------
;;;
;;; This section contains code that is called by CLOS to
;;;   update generic-function-call-history and to call
;;;   codegen-dispatcher to generate a new dispatch function when needed
;;;

(defun maybe-update-instances (arguments)
  (let ((invalid-instance nil))
    (dolist (i arguments)
      (when (and (core:instancep i)
                 (si:sl-boundp (si:instance-sig i)))
        (clos::with-early-accessors (+standard-class-slots+)
          (unless (= (core:instance-stamp i) (core:class-stamp-for-instances (core:instance-class i)))
            (gf-log "   instance-stamp matches that of class -> %s%N"  (= (core:instance-stamp i) (core:class-stamp-for-instances (core:instance-class i))))
            (gf-log "(core:instance-stamp i) -> %s%N" (core:instance-stamp i))
            (gf-log "(core:class-stamp-for-instances (core:instance-class i)) -> %s%N" (core:class-stamp-for-instances (core:instance-class i)))
            (setf invalid-instance t)
            (update-instance i)))))
    invalid-instance))

;; A call history selector a list of classes and eql-specializers
;; So to compute the applicable methods we need to consider the following cases for each
;; possible entry selector ...
;; 1) The selector is a list of just classes
;;      In that case behavior like that of compute-applicable-methods-using-classes can be used
;; 2) The selector is a mix of classes and eql-specializers
;;      In this case something like compute-applicable-methods needs to be used - but
;;         compute-applicable-methods takes a list of ARGUMENTS
;;        So I think we 
(defun applicable-method-p (method specializers)
  (loop for spec in (method-specializers method)
        for argspec in specializers
        always (cond ((eql-specializer-p spec)
                      (and (eql-specializer-p argspec)
                           (eql (eql-specializer-object argspec)
                                (eql-specializer-object spec))))
                     ((eql-specializer-p argspec)
                      (subclassp (class-of (eql-specializer-object argspec))
                                 spec))
                     (t (subclassp argspec spec)))))

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
      ;; should be impossible. one way I hit it: abnormal slots from boot.lsp
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
    (generic-function method-combination methods actual-specializers &key log)
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
                 ;; interpreter. See also, comment in combin.lsp.
                 ((and (consp em) (eq (first em) '%magic-no-required-method))
                  (gf-log "No-required-method as effective method%N")
                  (gf-log "em: %s%N" em)
                  (let ((group-name (second em)))
                    (make-effective-method-outcome
                     :methods methods :form em
                     :function (lambda (core:&va-rest vaslist-args)
                                 (apply #'no-required-method
                                        generic-function group-name vaslist-args)))))
                 (t
                  (gf-log "Using default effective method function%N")
                  (gf-log "(compute-effective-method generic-function method-combination methods) -> %N")
                  (gf-log "%s%N" em)
                  (make-effective-method-outcome
                   :methods methods :form em
                   :function (effective-method-function
                              em (gf-arg-info generic-function)))))))
    #+debug-fastgf
    (when log
      (gf-log "vvv************************vvv%N")
      (gf-log "compute-effective-method-function for %s%N" (generic-function-name generic-function))
      (gf-log "There are %d methods...%N" (length methods))
      (dolist (m methods)
        (gf-log "Method: %s %s %s%N" (clos::method-specializers m) (clos::method-qualifiers m) m))
      (gf-log "Effective method function -> %s%N" optimized)
      (gf-log "Effective method function address -> %s%N" (core:object-address optimized))
      (gf-log "^^^************************^^^%N"))
    optimized))

(defun outcome
    (generic-function method-combination methods actual-specializers)
  (or (find-existing-outcome (safe-gf-call-history generic-function)
                             methods)
      (compute-outcome
       generic-function method-combination methods actual-specializers)))

(defun update-call-history-for-add-method (generic-function orig-call-history method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to."
  (let ((call-history (copy-list orig-call-history)))
    (loop for entry in call-history
          for specializers = (coerce (car entry) 'list)
          when (applicable-method-p method specializers)
            do (let* ((methods (compute-applicable-methods-using-specializers
                                generic-function
                                specializers))
                      (outcome (outcome
                                generic-function
                                (generic-function-method-combination
                                 generic-function)
                                ;; I have actual specializers from the call history
                                ;; in specializers
                                methods specializers)))
                 (rplacd entry outcome)))
    call-history))

(defun update-generic-function-call-history-for-add-method (generic-function method)
  "When a method is added then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (loop for call-history = (safe-gf-call-history generic-function)
     for new-call-history = (update-call-history-for-add-method generic-function call-history method)
     for exchange = (safe-gf-call-history-cas generic-function call-history new-call-history)
     until (eq exchange call-history)))

(defun update-call-history-for-remove-method (generic-function call-history method)
  (let (new-call-history)
    (loop for entry in call-history
          for specializers = (coerce (car entry) 'list)
          if (applicable-method-p method specializers)
            do (let* ((methods (compute-applicable-methods-using-specializers
                                generic-function
                                specializers))
                      (outcome (if methods
                                   (outcome
                                    generic-function
                                    (generic-function-method-combination
                                     generic-function)
                                    methods specializers)
                                   nil)))
                 (when outcome
                   (push (cons (car entry) outcome) new-call-history)))
          else
            do (push (cons (car entry) (cdr entry)) new-call-history))
    new-call-history))

(defun update-generic-function-call-history-for-remove-method (generic-function method)
  "When a method is removed then we update the effective-method-functions for
   those call-history entries with specializers that the method would apply to
    AND if that means there are no methods left that apply to the specializers
     then remove the entry from the list.
FIXME!!!! This code will have problems with multithreading if a generic function is in flight. "
  (loop for call-history = (safe-gf-call-history generic-function)
        for new-call-history = (update-call-history-for-remove-method generic-function call-history method)
        for exchange = (safe-gf-call-history-cas generic-function call-history new-call-history)
        until (eq exchange call-history)))

;;; FIXME: Replace with atomic setf
(defun erase-generic-function-call-history (generic-function)
  (loop for call-history = (safe-gf-call-history generic-function)
        for new-call-history = nil
        for exchange = (safe-gf-call-history-cas generic-function call-history new-call-history)
        until (eq exchange call-history)))

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

(defun memoize-call (generic-function new-entry)
  "Memoizes the call and installs a new discriminator function - returns nothing"
  (let ((memoized-key (car new-entry)))
    (gf-log "Specializers key: %s%N" memoized-key)
    (gf-log "The specializer-profile: %s%N"
            (safe-gf-specializer-profile generic-function))
    (let ((specializer-profile (safe-gf-specializer-profile generic-function)))
      (unless (= (length memoized-key) (length specializer-profile))
        (error "The memoized-key ~a is not the same length as the specializer-profile ~a for ~a"
               memoized-key specializer-profile generic-function))
      (loop for call-history = (safe-gf-call-history generic-function)
            for found-key = (call-history-find-key call-history memoized-key)
            for new-call-history = (if found-key
                                       (progn
                                         (gf-log "Error: For generic function: %s - the key was already in the history - either bad dtree, incorrect lowering to llvm-ir or maybe (unlikely) put there by another thread.%N"
                                                 (generic-function-name generic-function))
                                         call-history)
                                       (progn
                                         (gf-log "Pushing entry into call history%N")
                                         (cons new-entry call-history)))
            for exchanged = (safe-gf-call-history-cas
                             generic-function call-history new-call-history)
            do (progn
                 #+debug-fastgf
                 (let ((specializer-profile
                         (safe-gf-specializer-profile generic-function)))
                   (when call-history
                     (gf-log "The dtree calculated for the call-history/specializer-profile:%N")
                     (gf-log "%s%N" (basic-tree call-history specializer-profile)))))
            when exchanged
              do (gf-log "Successfully exchanged call history%N")
            until (eq exchanged call-history))
      (loop for idx from 0 below (length memoized-key)
            for specializer = (svref memoized-key idx)
            unless (eql-specializer-p specializer)
              do (specializer-call-history-generic-functions-push-new
                  specializer generic-function))
      #+debug-long-call-history
      (when (> (length (generic-function-call-history generic-function)) 16384)
        (error "DEBUG-LONG-CALL-HISTORY is triggered - The call history for ~a is longer (~a entries) than 16384" generic-function (length (generic-function-call-history generic-function))))
      (force-dispatcher generic-function)
      (values))))

(defun perform-outcome (outcome arguments vaslist-arguments)
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
       (apply function vaslist-arguments)))
    (t (error "BUG: Bad thing to be an outcome: ~a" outcome))))

#+debug-fastgf
(defvar *dispatch-miss-start-time*)

(defun all-eql-specialized-p (specializer-profile arguments)
  ;; Make sure that any argument in a position with eql-specializers
  ;; is an eql specializer object.
  ;; If they all are, return a memoization key. If not, NIL.
  ;; See comment in do-dispatch-miss for rationale.
  (loop for spec across specializer-profile
        for arg in arguments
        collect (if (consp spec) ; this parameter is eql-specialized by some method
                    (if (member arg spec)
                        (intern-eql-specializer arg)
                        (return-from all-eql-specialized-p nil))
                    (class-of arg))
          into key
        finally (return (coerce key 'simple-vector))))

;;; Returns two values: the outcome to perform, and a new call history entry,
;;; or NIL if none should be added (e.g. due to eql specialization).
;;; This function has no side effects. DO-DISPATCH-MISS is in charge of that.
(defun dispatch-miss-info (generic-function arguments)
  (let ((argument-classes (mapcar #'class-of arguments)))
    (multiple-value-bind (method-list ok)
        (compute-applicable-methods-using-classes generic-function argument-classes)
      (gf-log "Called compute-applicable-methods-using-classes - returned method-list: %s  ok: %s%N"
              method-list ok)
      (let* ((method-list (if ok
                              method-list
                              (compute-applicable-methods generic-function arguments)))
             (final-methods (final-methods method-list argument-classes))
             (outcome (outcome
                       generic-function
                       (generic-function-method-combination generic-function)
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
                (let ((key-length
                        (length (safe-gf-specializer-profile generic-function))))
                  (cons (coerce (subseq argument-classes 0 key-length) 'simple-vector)
                        outcome)))
               ((eq (class-of generic-function) (find-class 'standard-generic-function))
                ;; we have a call with eql specialized arguments.
                ;; We can still memoize this sometimes, as long as the gf is standard.
                ;; What we need to watch out for it the following situation-
                ;; (defmethod foo ((x (eql 'x))) ...)
                ;; (foo 'y)
                ;; If we memoize this naively, we'll put in an entry for class SYMBOL,
                ;; and then if we call (foo 'x) later, it will go to that instead of properly
                ;; missing the cache.
                ;; EQL specializers play merry hob hell with the assumption of fastgf that
                ;; as long as you treat all classes distinctly there are no problems with
                ;; inheritance, basically.
                ;; So, we only memoize the (foo 'x) call, and leave the more general symbol
                ;; specialization to slow path. We could fix it up further to have that
                ;; fast-path as well, but I think it's more involved, and probably not worth it,
                ;; as most functions with eql specialization
                ;; only use that argument with eql specializer objects, or so I assume.
                (let ((maybe-memo-key
                        (all-eql-specialized-p
                         (generic-function-specializer-profile generic-function)
                         arguments)))
                  (cond (maybe-memo-key
                         (gf-log-dispatch-miss "Memoizing eql-specialized call"
                                               generic-function arguments)
                         (cons maybe-memo-key outcome))
                        (t
                         (gf-log-dispatch-miss "Cannot memoize eql-specialized call"
                                               generic-function arguments)
                         nil))))
               (t
                ;; No more options: we just don't memoize.
                ;; This only occurs with eql specializers,
                ;; at least with the standard c-a-m/-u-c methods.
                (gf-log-dispatch-miss "Cannot memoize call" generic-function arguments)
                nil)))))))

(defun do-dispatch-miss (generic-function vaslist-arguments arguments)
  "This effectively does what compute-discriminating-function does and maybe memoizes the result
and calls the effective-method-function that is calculated.
It takes the arguments in two forms, as a vaslist and as a list of arguments."
  (multiple-value-bind (min max) (generic-function-min-max-args generic-function)
    (let ((nargs (length arguments)))
      (when (or (< nargs min) (and max (> nargs max)))
        (error 'core:wrong-number-of-arguments
               :called-function generic-function :given-nargs nargs
               :min-nargs min :max-nargs max))))
  (let (#+debug-fastgf
        (*dispatch-miss-start-time* (get-internal-real-time)))
    (multiple-value-bind (outcome new-ch-entry)
        (dispatch-miss-info generic-function arguments)
      (when new-ch-entry
        (memoize-call generic-function new-ch-entry))
      (gf-log "Performing outcome %s%N" outcome)
      #+debug-fastgf
      (let ((results (multiple-value-list
                      (perform-outcome outcome arguments vaslist-arguments))))
        (gf-log "+-+-+-+-+-+-+-+-+ do-dispatch-miss done real time: %f seconds%N" (/ (float (- (get-internal-real-time) *dispatch-miss-start-time*)) internal-time-units-per-second))
        (gf-log "----}---- Completed call to effective-method-function for %s results -> %s%N" (clos::generic-function-name generic-function) results)
        (values-list results))
      #-debug-fastgf
      (perform-outcome outcome arguments vaslist-arguments))))

(defun dispatch-miss (generic-function core:&va-rest valist-args)
  (core:stack-monitor (lambda () (format t "In clos::dispatch-miss with generic function ~a~%"
                                         (clos::generic-function-name generic-function))))
  (#+debug-fastgf unwind-protect #-debug-fastgf multiple-value-prog1
       (progn
         (incf-debug-fastgf-indent)
         ;; Update any invalid instances
         (let* ((arguments (core:list-from-va-list valist-args))
                (invalid-instance (maybe-update-instances arguments)))
           (if invalid-instance
               (apply generic-function valist-args)
               (progn
                 #+debug-fastgf
                 (progn
                   (gf-log "----{---- A dispatch-miss occurred[(1- (core:next-number))->%s]  -> %s  %N" (1- (core:next-number)) (clos::generic-function-name generic-function))
                   (dolist (arg (core:list-from-va-list valist-args))
                     (gf-log "%s[%s/%d] " (core:safe-repr arg) (core:safe-repr (class-of arg)) (core:instance-stamp arg)))
                   (gf-log-noindent "%N"))
                 (do-dispatch-miss generic-function valist-args arguments)))))
    (decf-debug-fastgf-indent)))

;;; Called from the dtree interpreter, because APPLY from C++ is kind of annoying.
(defun dispatch-miss-va (generic-function valist-args)
  (apply #'dispatch-miss generic-function valist-args))

(defvar *fastgf-force-compiler* nil)
(defun calculate-fastgf-dispatch-function (generic-function &key compile)
  (if (safe-gf-call-history generic-function)
      (let ((timer-start (get-internal-real-time)))
        (unwind-protect
             (if (or *fastgf-force-compiler* compile)
                 (cmp:bclasp-compile
                  nil (generate-discriminator generic-function))
                 (interpreted-discriminator generic-function))
          (let ((delta-seconds (/ (float (- (get-internal-real-time) timer-start) 1d0)
                                  internal-time-units-per-second)))
            (gctools:accumulate-discriminating-function-compilation-seconds delta-seconds))))
      (invalidated-discriminating-function-closure generic-function)))

(defun force-dispatcher (generic-function)
  (let (log-output)
    #+debug-fastgf
    (progn
      (if (eq (class-of generic-function) (find-class 'standard-generic-function))
          (let ((generic-function-name (core:low-level-standard-generic-function-name generic-function)))
            (setf log-output (log-cmpgf-filename generic-function-name "func" "ll"))
            (gf-log "Writing dispatcher to %s%N" log-output))
          (setf log-output (log-cmpgf-filename (generic-function-name generic-function) "func" "ll")))
      (incf-debug-fastgf-didx))
    (set-funcallable-instance-function generic-function
                                       (calculate-fastgf-dispatch-function
                                        generic-function))))

;;; Used by interpret-dtree-program.
(defun compile-discriminating-function (generic-function)
  (set-funcallable-instance-function generic-function
                                     (calculate-fastgf-dispatch-function
                                      generic-function :compile t)))

#+debug-fastgf
(defvar *dispatch-miss-recursion-check* nil)

(defun invalidated-dispatch-function (generic-function valist-args)
  (declare (optimize (debug 3)))
  #+debug-fastgf
  (when (find (cons generic-function (core:list-from-va-list valist-args)) *dispatch-miss-recursion-check*
              :test #'equal)
    (format t "~&Recursive dispatch miss detected~%")
    (ext:btcl)
    (ext:quit 1))
  (let (#+debug-fastgf
        (*dispatch-miss-recursion-check* (cons (cons generic-function
                                                     (core:list-from-va-list valist-args))
                                               *dispatch-miss-recursion-check*)))

  ;;; If there is a call history then compile a dispatch function
  ;;;   being extremely careful NOT to use any generic-function calls.
  ;;;   Then redo the call.
  ;;; If there is no call history then treat this like a dispatch-miss.
  #+debug-fastgf
  (if (eq (class-of generic-function) (find-class 'standard-generic-function))
      (gf-log "Entered invalidated-dispatch-function for %s - avoiding generic function calls until return!!!%N"
              (core:low-level-standard-generic-function-name generic-function))
      (gf-log "Entered invalidated-dispatch-function - avoiding generic function calls until return!!!%N"))
  (gf-log "Specializer profile is %s%N" (safe-gf-specializer-profile generic-function))
  (if (safe-gf-call-history generic-function)
      (progn
        (force-dispatcher generic-function)
        (apply generic-function valist-args))
      (apply #'dispatch-miss generic-function valist-args))))

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
  (loop for entry in (safe-gf-call-history gf)
     unless (call-history-entry-involves-method-with-specializers entry method-specializers)
       collect entry))

#+(or)
(defun call-history-after-class-change (gf class)
;;;  (format t "call-history-after-class-change  start: gf->~a  call-history ->~a~%" gf (clos::generic-function-cal-history gf))
  (loop for entry in (safe-gf-call-history gf)
     unless (loop for subclass in (subclasses* class)
               thereis (call-history-entry-key-contains-specializer (car entry) subclass))
     collect entry))

(defun subclasses* (class)
  (remove-duplicates
   (cons class
         (reduce #'append (mapcar #'subclasses*
                                  (class-direct-subclasses class))))))

(defun call-history-entry-key-contains-specializers (key specializers)
  (loop for specializer in specializers
        do (when (find specializer key :test #'eq)
             (return-from call-history-entry-key-contains-specializers t))))

(defun generic-function-call-history-separate-entries-with-specializers (gf call-history specializers)
  (gf-log "generic-function-call-history-remove-entries-with-specializers  gf: %s%N    specializers: %s%N" gf specializers)
  #+debug-long-call-history
  (when (> (length call-history) 16384)
    (error "DEBUG-LONG-CALL-HISTORY is triggered - The call history for ~a is longer (~a entries) than 16384" gf (length call-history)))
  (let ((removed nil) (keep nil))
    (loop for entry in call-history
          for key = (car entry)
          do (gf-log "         check if entry key: %s   contains specializer: %s%N" key specializers)
          if (call-history-entry-key-contains-specializers key specializers)
            do (progn
                 (gf-log "       It does - removing entry%N")
                 (push entry removed))
          else do (progn
                    (gf-log "       It does not - keeping entry%N")
                    (push entry keep)))
    #+debug-long-call-history
    (progn
      (unless (<= (length keep ) (length call-history))
        (error "The number of call history entries to keep (~a) MUST be less than the number of call-history entries ~a~%" (length keep) (length call-history)))
      (unless (<= (length removed ) (length call-history))
        (error "The number of call history entries removed (~a) MUST be less than the number of call-history entries ~a~%" (length removed) (length call-history)))
      (unless (= (+ (length removed) (length keep)) (length call-history))
        (error "The sum of removed and kept entries (+ ~a ~a) -> ~a does not equal the number of call-history entries ~a~%" (length removed) (length keep) (+ (length removed) (length keep)) (length call-history))))
    (values keep removed)))

(defun invalidate-generic-functions-with-class-selector (top-class)
  (gf-log "invalidate-generic-functions-with-class-specializer %s%N" top-class)
  (let* ((all-subclasses (subclasses* top-class))
         (_ (gf-log "  %d subclasses*%N" (length all-subclasses)))
         (_ (gf-log "        %s%N" all-subclasses))
         (generic-functions
           (loop for subclass in all-subclasses
                 for spec-generic-functions = (specializer-call-history-generic-functions subclass)
                 do (gf-log "   for subclass %s there are %d spec-generic-functions%N" subclass (length spec-generic-functions))
                 do (gf-log "         spec-generic-functions -> %s%N" spec-generic-functions)
                 append spec-generic-functions))
         (_ (gf-log "  %d generic-functions...%N" (length generic-functions)))
         (_ (gf-log "        %s%N" generic-functions))
         (unique-generic-functions (remove-duplicates generic-functions))
         (_ (gf-log "  unique-generic-functions...%N"))
         (_ (gf-log "        %s%N" unique-generic-functions))
         edited)
    (gf-log "   subclasses* -> %s%N" all-subclasses)
    ;;(when core:*debug-dispatch* (format t "    generic-functions: ~a~%" generic-functions))
    (loop for gf in unique-generic-functions
          do (gf-log "generic function: %s%N" (clos:generic-function-name gf))
             (gf-log "    (clos:get-funcallable-instance-function gf) -> %s%N"
                     (clos:get-funcallable-instance-function gf))
             (loop for call-history = (safe-gf-call-history gf)
                   for new-call-history
                     = (generic-function-call-history-separate-entries-with-specializers
                        gf call-history all-subclasses)
                   for exchange
                     = (safe-gf-call-history-cas gf call-history new-call-history)
                   until (eq exchange call-history)
                   finally (gf-log "    edited call history%N")
                           (gf-log "%s%N" new-call-history)
                           (gf-log "Generating a new discriminating function%N")
                           (if new-call-history
                               (force-dispatcher gf)
                               (invalidate-discriminating-function gf)))
          #+debug-fastgf
           (let (log-output)  
             (progn
               (if (eq (class-of gf) (find-class 'standard-generic-function))
                   (let ((generic-function-name (core:low-level-standard-generic-function-name gf)))
                     (setf log-output (log-cmpgf-filename generic-function-name "func" "ll"))
                     (gf-log "Writing dispatcher to %s%N" log-output))
                   (setf log-output (log-cmpgf-filename (generic-function-name gf) "func" "ll")))
               (incf-debug-fastgf-didx))))))

;;; This is called by the dtree interpreter when it doesn't get enough arguments,
;;; because computing this stuff in C++ would be needlessly annoying.
(defun interp-wrong-nargs (generic-function given-nargs)
  (multiple-value-bind (min max) (generic-function-min-max-args generic-function)
    (error 'core:wrong-number-of-arguments
           :called-function generic-function :given-nargs given-nargs
           :min-nargs min :max-nargs max)))
