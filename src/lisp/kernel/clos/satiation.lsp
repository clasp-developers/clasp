(in-package "CLOS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satiation of generic functions to start fastgf
;;;
;;; Ideas copied from Sicl/Code/CLOS/satiation.lisp
;;;

;;; Essentially, for some gfs we need in a consistent state for the system to work,
;;; during boot we fake a call history so that they can be called without invoking
;;; gfs such as themselves that haven't yet been placed in a working state.
;;; A fake call history is also nice for efficiency - if we install an anticipated
;;; history beforehand, we can avoid repeatedly compiling new discriminators.

;;; In order to do things both at boot and in an exported interface, we duplicate
;;; some code. Unfortunate but I don't see a good way to avoid it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UTILITY
;;;

;;; Add a portion of call history into a gf's existing call history.
;;; If any entry to be added duplicates an existing entry, the new entry prevails.
(defun append-generic-function-call-history (generic-function new-entries)
  (loop for call-history = (generic-function-call-history generic-function)
        ;; By keeping the new entry, remove-if will return immediately in the
        ;; usual case that the existing history is empty.
        for cleaned-call-history = (remove-if (lambda (entry)
                                                (call-history-find-key
                                                 new-entries (car entry)))
                                              call-history)
        for new-history = (append new-entries cleaned-call-history)
        for exchange = (generic-function-call-history-compare-exchange
                        generic-function call-history new-history)
        until (eq exchange new-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BOOT-TIME SATIATION

;;; Satiation should occur before any generic function calls are performed, and
;;; involve no generic function calls.

;;; Note that the accessors for call-history and specializer-profile are not accessors,
;;; they are C++ functions. So they're okay to use without early-accessors.

;;; effective-slot-from-accessor-method, but without gfs
(defun early-effective-slot-from-accessor-method (method class)
  (with-early-accessors (+standard-accessor-method-slots+
                         +slot-definition-slots+
                         +class-slots+)
    (let* ((direct-slot-definition (accessor-method-slot-definition method))
           (direct-slot-name (slot-definition-name direct-slot-definition))
           (slot (loop for effective-slot in (class-slots class)
                       when (eq direct-slot-name (slot-definition-name effective-slot))
                         return effective-slot)))
      (if slot
          slot
          (error "Bug during satiation: Could not find effective slot definition for ~a"
                 direct-slot-name)))))

;;; Used by add-satiation-entries to compute an effective method without going through
;;; compute-effective-method and method-combinations, which call a few gfs.
;;; Standard method combination only. No qualifiers allowed (checks this).
;;; It could be improved to handle them; I just don't think we need to satiate anything
;;; that uses qualified methods.
;;; Does handle accessor methods so that they're fast, yet.
;;; FIXME: Duplication of other code, in this case compute-outcome, sucks.
(defun early-compute-outcome (methods specializers)
  (with-early-accessors (+standard-method-slots+
                         +slot-definition-slots+)
    (mapc (lambda (method)
            (when (method-qualifiers method)
              ;; Hopefully the write won't trigger a recursive error...?
              (error "Bug during satiation: method to be satiated ~a has qualifiers"
                     method)))
          methods)
    ;; Methods are sorted by std-compute-applicable-methods-using-classes, so
    ;; we're just doing (call-method ,first (,@rest))
    (let* ((first (first methods)) (rest (rest methods))
           (efm (combine-method-functions
                 (method-function first)
                 ;; with-early-accessors does macrolet, hence this awkwardness.
                 (mapcar (lambda (method) (method-function method)) rest))))
      ;; realistically, anything we satiate is going to be standard classes, but
      ;; paranoia doesn't hurt here.
      (cond ((eq (class-of first) (find-class 'standard-reader-method))
             (let* ((class (first specializers))
                    (slot (early-effective-slot-from-accessor-method
                           first (first specializers))))
               (make-optimized-slot-reader :index (slot-definition-location slot)
                                                :effective-method-function efm
                                                :slot-name (slot-definition-name slot)
                                                :method first
                                                :class class)))
            ((eq (class-of first) (find-class 'standard-writer-method))
             (let* ((class (second specializers))
                    (slot (early-effective-slot-from-accessor-method
                           first (first specializers))))
               (make-optimized-slot-writer :index (slot-definition-location slot)
                                                :effective-method-function efm
                                                :slot-name (slot-definition-name slot)
                                                :method first
                                                :class class)))
            ((leaf-method-p first)
             (if (fast-method-function first)
                 (make-fast-method-call :function (fast-method-function first))
                 (method-function first)))
            (t efm)))))

;;; Add fictitious call history entries.
(defun add-satiation-entries (generic-function lists-of-specializers)
  (with-early-accessors (+standard-generic-function-slots+) ; for -method-combination
    (let ((new-entries
            (loop for specific-specializers in lists-of-specializers
                  for methods = (std-compute-applicable-methods-using-classes
                                 generic-function specific-specializers)
                  ;; Everything should use standard method combination during satiation.
                  for outcome = (early-compute-outcome methods specific-specializers)
                  collect (cons (coerce specific-specializers 'vector) outcome))))
      (append-generic-function-call-history generic-function new-entries))))

(defun early-satiate (generic-function &rest lists-of-specializers)
  ;; Many generic functions at startup will be missing specializer-profile at startup
  ;;    so we compute one here using the number of required arguments in the lambda-list.
  ;; The call-history may be incorrect because of improper initialization as
  ;;    clos starts up - so lets wipe it out and then satiate it.
  (gf-log "Starting satiate-generic-function%N")
  ;; Wipe out the call-history and satiate it using methods
  (gf-log "About to set call history%N")
  (erase-generic-function-call-history generic-function)
  (add-satiation-entries generic-function lists-of-specializers)
  ;; Now when the function is called the discriminating-function will be invalidated-dispatch-function
  ;; This well set up the real discriminating function. This shouldn't involve a dispatch miss, and
  ;; no generic-function calls (other than the one for the actual call, of course).
  (invalidate-discriminating-function generic-function))

;;; Satiate the minimum set of functions to make the system work.
;;; Essentially those that are used to compute new call history entries in the full system.
(defun satiate-minimal-generic-functions ()
  (macrolet ((satiate-one (gf-name &body lists-of-class-names)
               `(prog2
                    (gf-log ,(concatenate 'string "Satiating " (string gf-name) "%N"))
                    (early-satiate
                     (fdefinition ',gf-name)
                     ,@(loop for list in lists-of-class-names
                             collect `(list ,@(loop for name in list
                                                    collect `(find-class ',name)))))
                  (gf-log ,(concatenate 'string "Done satiating " (string gf-name) "%N")))))
    (satiate-one class-slots
                 (standard-class)
                 (funcallable-standard-class))
    ;; instance updates we shouldn't need to satiate...
    (satiate-one compute-applicable-methods-using-classes
                 (standard-generic-function cons)
                 (standard-generic-function null)) ; nulls may not be necessary, but no big.
    (satiate-one compute-applicable-methods
                 (standard-generic-function cons)
                 (standard-generic-function null))
    (satiate-one compute-effective-method
                 (standard-generic-function method-combination cons)
                 (standard-generic-function method-combination null))
    ;; We should satiate the method combination accessors, but we actually
    ;; just use early accessors at the moment... which is probably wrong (FIXME?)
    (satiate-one method-qualifiers     ; called by method combinations
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one method-specializers
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one method-function
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one accessor-method-slot-definition
                 (standard-reader-method) (standard-writer-method))
    (satiate-one slot-definition-allocation
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-name
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-location
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one generic-function-name
                 (standard-generic-function))
    (satiate-one generic-function-method-combination
                 (standard-generic-function))
    ;; This one is needed for the initial specializer profile computation in fixup.
    ;; (i.e., it's called by initialize-generic-function-specializer-profile)
    (satiate-one generic-function-lambda-list
                 (standard-generic-function))
    (satiate-one leaf-method-p
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))
    (satiate-one fast-method-function
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))))

;;; This function sets up an initial specializer profile for a gf that doesn't have one.
;;; It can only not have one if it was defined unnaturally, i.e. during boot.
;;; We have to call this on all generic functions so defined, so more than the ones that
;;; we satiate.
;;; Furthermore, we have to do so before any generic functions are called. That's why
;;; we use this separate function rather than compute-and-set-specializer-profile.
;;; FIXME: Since this is only called during boot, we probably only need one compare-exchange.
(defun setup-specializer-profile (proto-gf)
  ;; proto-gf in that it's unnaturally defined. it's still a standard-generic-function object.
  (with-early-accessors (+standard-generic-function-slots+
                         +standard-method-slots+)
    (unless (slot-boundp proto-gf 'lambda-list)
      (error "In setup-specializer-profile - ~s has no lambda list!"
             (core:low-level-standard-generic-function-name proto-gf)))
    (let* ((ll (generic-function-lambda-list proto-gf))
           ;; FIXME: l-l-r-a is defined in generic.lsp, which is after this, so we get a style warning.
           ;; (this function is not actually called until fixup)
           (new-profile (make-array (length (lambda-list-required-arguments ll))
                                    :initial-element nil)))
      (loop for old-profile = (generic-function-specializer-profile proto-gf)
            for exchange = (generic-function-specializer-profile-compare-exchange proto-gf old-profile new-profile)
            until (eq exchange new-profile)))
    (let ((methods (generic-function-methods proto-gf)))
      (if methods
          (loop for method in methods
                for specializers = (method-specializers method)
                ;; in closfastgf. we rely on this function not calling gfs.
                do (update-specializer-profile proto-gf specializers))
          (error "In setup-specializer-profile - ~s has no methods!"
                 (core:low-level-standard-generic-function-name proto-gf))))
    (gf-log "Set initial specializer profile for satiated function %s to %s%N"
            (core:low-level-standard-generic-function-name proto-gf)
            (generic-function-specializer-profile proto-gf))))

;;; Used in fixup for the %satiate macroexpansions (below).
(defun early-find-method (gf qualifiers specializers &optional (errorp t))
  (declare (notinline method-qualifiers))
  (with-early-accessors (+eql-specializer-slots+
                         +standard-generic-function-slots+
                         +standard-method-slots+)
    (flet ((filter-specializer (name)
             (cond ((typep name 'specializer)
                    name)
                   ((atom name)
                    (let ((class (find-class name nil)))
                      (unless class
                        (error "~A is not a valid specializer name" name))
                      class))
                   ((and (eq (first name) 'EQL)
                         (null (cddr name)))
                    (cdr name))
                   (t
                    (error "~A is not a valid specializer name" name))))
           (specializer= (cons-or-class specializer)
             (if (consp cons-or-class)
                 (and (eql-specializer-flag specializer)
                      (eql (car cons-or-class)
                           (eql-specializer-object specializer)))
                 (eq cons-or-class specializer))))
      (when (/= (length specializers)
                (length (generic-function-argument-precedence-order gf)))
        (error
         "The specializers list~%~A~%does not match the number of required arguments in ~A"
         specializers (core:low-level-standard-generic-function-name gf)))
      (loop with specializers = (mapcar #'filter-specializer specializers)
            for method in (generic-function-methods gf)
            when (and (equal qualifiers (method-qualifiers method))
                      (every #'specializer= specializers (method-specializers method)))
              do (return-from early-find-method method))
      ;; If we did not find any matching method, then the list of
      ;; specializers might have the wrong size and we must signal
      ;; an error.
      (when errorp
        (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
               (core:low-level-standard-generic-function-name gf)
               qualifiers specializers)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISCRIMINATING FUNCTIONS IN COMPILE-FILE
;;;

(defun wrap-call-method (effective-method method-function-map)
  `(macrolet ((call-method (method &optional nexts)
                (if (consp method)
                    (second method) ; (make-method form)
                    (let ((mfs ',method-function-map))
                      (flet ((mf (method)
                               (or (cdr (assoc method mfs))
                                   (error "BUG: Horrible things are occurring."))))
                        `(funcall ,(mf method) .method-args.
                                  (list
                                   ,@(loop for next in nexts
                                           collect (if (consp next) ; make-method
                                                       `(lambda (.method-args. .next-methods.)
                                                          (declare (ignore .next-methods.))
                                                          ,(second next))
                                                       (mf next))))))))))
     ,effective-method))

;;; FIXME: duplicates satiated-call-history heavily. not sure of the best way to fix it,
;;; since this is a "runtime" history (for the compiler) and that's a "compile time" history.
(defun compile-time-call-history (generic-function &rest lists-of-specializer-names)
  (loop with mc = (generic-function-method-combination generic-function)
        with fmf-binds = nil
        with mf-binds = nil
        with mfo-binds = nil
        with emf-binds = nil
        for list-of-specializer-names in lists-of-specializer-names
        for list-of-specializers
          = (coerce-to-list-of-specializers list-of-specializer-names)
        for am = (compute-applicable-methods-using-specializers
                  generic-function list-of-specializers)
        for em = (compute-effective-method generic-function mc am)
        for method = (and (consp em) (eq (first em) 'call-method) (second em))
        for leafp = (when method (leaf-method-p method))
        ;; this is just whether there is a fast method function, not the function
        ;; itself, which is not required. Indicated with this booleanization.
        for fmf = (when leafp
                    (if (fast-method-function method) t nil))
        for readerp = (when method (eq (class-of method) (find-class 'standard-reader-method)))
        for writerp = (when method (eq (class-of method) (find-class 'standard-writer-method)))
        for accessor-class = (cond ((not method) nil)
                                   (readerp (first list-of-specializers))
                                   (writerp (second list-of-specializers))
                                   (t nil))
        for slotd = (when accessor-class
                      (effective-slotd-from-accessor-method method accessor-class))
        for standard-slotd-p = (when slotd
                                 (eq (class-of slotd)
                                     (find-class 'standard-effective-slot-definition)))
        do (when (null am)
             (error "No applicable methods for SATIATE of ~a with ~a"
                    generic-function list-of-specializer-names))
        do (when (and (consp em) (eq (first em) 'no-required-method))
             (error "No required methods for SATIATE of ~a with ~a"))
        collect (cons (coerce list-of-specializers 'vector)
                      (cond
                        (standard-slotd-p
                         (cond (readerp (make-optimized-slot-reader
                                         :index (slot-definition-location slotd)
                                         :slot-name (slot-definition-name slotd)
                                         :method method
                                         :class accessor-class))
                               (writerp (make-optimized-slot-writer
                                         :index (slot-definition-location slotd)
                                         :slot-name (slot-definition-name slotd)
                                         :method method
                                         :class accessor-class))
                               (t (error "BUG: Unreachable weirdness in SATIATE for ~a, ~a"
                                         generic-function list-of-specializer-names))))
                        (fmf (or (cdr (assoc method fmf-binds))
                                 (let* ((sym (gensym "FAST-METHOD-FUNCTION"))
                                        (outcome (make-fast-method-call :function sym)))
                                   (push (cons method outcome) fmf-binds)
                                   outcome)))
                        (leafp (or (cdr (assoc method mfo-binds))
                                   (let* ((mf (or (cdr (assoc method mf-binds))
                                                  (let ((sym (gensym "METHOD-FUNCTION")))
                                                    (push (cons method sym) mf-binds)
                                                    sym)))
                                          (outcome (make-function-outcome :function mf)))
                                     (push (cons method outcome) mfo-binds)
                                     outcome)))
                        (t (or (cdr (assoc am emf-binds :test #'equal))
                               (let* ((sym (gensym "EFFECTIVE-METHOD-FUNCTION"))
                                      (local-mf
                                        (loop for m in am
                                              collect (cons m
                                                            (or (cdr (assoc m mf-binds))
                                                                (let ((sym (gensym "METHOD-FUNCTION")))
                                                                  (push (cons m sym) mf-binds)
                                                                  sym)))))
                                      (outcome (make-effective-method-outcome
                                                :applicable-methods am
                                                :function (wrap-call-method em local-mf))))
                                 (push (cons am outcome) emf-binds)
                                 outcome)))))
          into entries
        finally (return (values entries
                                (loop for (key . val) in fmf-binds
                                      collect (cons key (fast-method-call-function val)))
                                mf-binds))))

(defun method-specializers-form (specializers)
  `(list ,@(loop for s in specializers
                 collect (etypecase s
                           (eql-specializer
                            `(intern-eql-specializer
                             ',(eql-specializer-object s)))
                           (class s)))))

;; For satiation of CLOS, we need to use early-find-method and so on to prevent failures.
;; If this is T, we use those. Otherwise we use the standard stuff.
;; Note that this isn't complete - we also need those macro bindings in fixup.lsp.
(defvar *early-satiation* nil)

(defun find-method-form (method)
  `(,(if *early-satiation* 'early-find-method 'find-method)
    (fdefinition ',(generic-function-name (method-generic-function method)))
    ',(method-qualifiers method)
    ,(method-specializers-form (method-specializers method))))

(defun compile-time-bindings-junk (fmf-binds mf-binds)
  (append (loop for (method . name) in fmf-binds
                collect (list name
                              (if *early-satiation*
                                  `(load-time-value
                                    (with-early-accessors (+standard-method-slots+)
                                      (fast-method-function ,(find-method-form method))))
                                  `(load-time-value
                                    (fast-method-function ,(find-method-form method))))))
          (loop for (method . name) in mf-binds
                collect (list name
                              (if *early-satiation*
                                  `(load-time-value
                                    (with-early-accessors (+standard-method-slots+)
                                      (method-function ,(find-method-form method))))
                                  `(load-time-value
                                    (method-function ,(find-method-form method))))))))

(defun compile-time-discriminator (generic-function &rest lists-of-specializer-names)
  (multiple-value-bind (call-history fmf-binds mf-binds)
      (apply #'compile-time-call-history generic-function lists-of-specializer-names)
    (let ((name (generic-function-name generic-function)))
      (generate-dispatcher-from-dtree
       generic-function
       (calculate-dtree call-history (generic-function-specializer-profile generic-function))
       :extra-bindings (compile-time-bindings-junk fmf-binds mf-binds)
       :generic-function-name name
       :generic-function-form `(load-time-value (fdefinition ',name) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERAL SATIATION INTERFACE

;;; Main entry point
;;; Actually quite simple.
;;; Possible improvement: Put in some handler-case stuff to reduce any errors to
;;; warnings, given that this is just for optimization.
(defun satiate (generic-function &rest lists-of-specializer-designators)
  (flet ((coerce-specializer-designator (specializer-designator)
           ;; The fake EQL specializers fastgf uses.
           ;; (It's getting annoying. FIXME?)
           (etypecase specializer-designator
             (eql-specializer (list (eql-specializer-object specializer-designator)))
             (specializer specializer-designator)
             (symbol (find-class specializer-designator))
             ((cons (eql eql) (cons t null)) ; (eql thing)
              (list (second specializer-designator))))))
    (loop with method-combination = (generic-function-method-combination generic-function)
          for list in lists-of-specializer-designators
          for specializers = (mapcar #'coerce-specializer-designator list)
          for applicable-methods
            = (compute-applicable-methods-using-specializers generic-function specializers)
          for outcome = (compute-outcome generic-function method-combination
                                         applicable-methods specializers)
          collect (cons (coerce specializers 'simple-vector) outcome) into history
          finally (append-generic-function-call-history generic-function history))))

;;; The less simple part is doing things at compile-time.
;;; We can't put discriminating functions into FASLs because they include the class stamps,
;;; which are hard to synchronize between compile- and load-time.
;;; But we can dump (invented) call histories, including ones with functions in them.
;;; It's somewhat convoluted however.
;;; Anywho, we do that for SATIATE calls provided
;;; 1) the GENERIC-FUNCTION form is #'foo
;;; 2) all the lists of specializer designators are constant.
;;; At the moment, it also requires that the generic function and all relevant methods are defined
;;; at compile time. If we store information about DEFMETHODs in the environment at compile time,
;;; though, that shouldn't be required. (EQL specializers will be a bit weird, though.)

(define-compiler-macro satiate (&whole form generic-function &rest lists &environment env)
  (if (and (consp generic-function)
           (eq (car generic-function) 'function)
           (consp (cdr generic-function))
           (null (cddr generic-function))
           (fboundp (second generic-function))
           (loop for list in lists always (constantp list env)))
      `(%satiate ,(second generic-function)
                 ,@(mapcar (lambda (form) (ext:constant-form-value form env)) lists))
      form))

;; Given a list of specializer names or specializers, returns a list of specializers.
(defun coerce-to-list-of-specializers (list-of-specializer-names)
  (mapcar (lambda (sname)
            (etypecase sname
              (class sname)
              (eql-specializer
               (list (eql-specializer-object sname)))
              ;; (eql 'something)
              ((cons (eql eql)
                     (cons t null))
               ;; The fake EQL specializers used by c-a-m-u-s
               (list (second sname)))
              (symbol (find-class sname))))
          list-of-specializer-names))

(defmacro satiated-call-history (generic-function-name &rest lists-of-specializer-names)
  (let (method-binds method-function-binds method-function-outcome-binds emfo-binds fmf-binds)
    ;;; Confusing and complicated macros.
    ;;; Here's what they do: (ensure-method variable) expands into something that checks method-binds
    ;;; for the value of variable. If it's there, that's returned. If not, a new gensym is made and put
    ;;; there, along with the default form.
    ;;; Basically it ensures that there's a binding of the value available for the expansion below.
    (macrolet ((ensure (key var default namething)
                 ;; EQUAL test for ensure-emfo
                 `(let ((entry (assoc ,key ,var :test #'equal)))
                    (if (consp entry)
                        (second entry)
                        (let ((name (gensym ,namething)))
                          (push (list ,key name ,default) ,var)
                          name))))
               (ensure-method (method)
                 `(ensure ,method method-binds
                          ;; FIXME: We use DEFINITION instead of #' because we have local macros
                          ;; for method-qualifiers etc., which are also satiated (and need to refer to)
                          `(find-method (fdefinition ',generic-function-name)
                                        ',(method-qualifiers ,method)
                                        ,(method-specializers-form (method-specializers ,method)))
                          "METHOD"))
               (ensure-method-function (method)
                 `(ensure ,method method-function-binds
                          `(method-function ,(ensure-method ,method))
                          "METHOD-FUNCTION"))
               (ensure-method-function-outcome (method)
                 `(ensure ,method method-function-outcome-binds
                          `(make-function-outcome :function ,(ensure-method-function ,method))
                          "METHOD-FUNCTION-OUTCOME"))
               (ensure-emfo (method-list effective-method)
                 `(ensure ,method-list emfo-binds
                          `(make-effective-method-outcome
                            :applicable-methods (list ,@(loop for method in ,method-list
                                                              collect (ensure-method method)))
                            :function (lambda (.method-args. .next-methods.)
                                        (declare (ignore .next-methods.))
                                        ,,effective-method))
                          "EFFECTIVE-METHOD-OUTCOME"))
               (ensure-fmf (method)
                 `(ensure ,method fmf-binds
                          `(make-fast-method-call
                            :function (fast-method-function ,(ensure-method ,method)))
                          "FAST-METHOD-FUNCTION")))
      (let* ((generic-function (fdefinition generic-function-name))
             (mc (generic-function-method-combination generic-function))
             (entries
               (loop for list-of-specializer-names in lists-of-specializer-names
                     for list-of-specializers
                       = (coerce-to-list-of-specializers list-of-specializer-names)
                     for am = (compute-applicable-methods-using-specializers
                               generic-function list-of-specializers)
                     for em = (compute-effective-method generic-function mc am)
                     ;; time to recapitulate compute-outcome.
                     for method = (and (consp em) (eq (first em) 'call-method) (second em))
                     for leafp = (when method (leaf-method-p method))
                     for fmf = (when leafp (fast-method-function method))
                     for readerp = (when method (eq (class-of method) (find-class 'standard-reader-method)))
                     for writerp = (when method (eq (class-of method) (find-class 'standard-writer-method)))
                     for accessor-class = (cond ((not method) nil)
                                                (readerp (first list-of-specializers))
                                                (writerp (second list-of-specializers))
                                                (t nil))
                     for slotd = (when accessor-class
                                   (effective-slotd-from-accessor-method method accessor-class))
                     for standard-slotd-p = (when slotd
                                              (eq (class-of slotd)
                                                  (find-class 'standard-effective-slot-definition)))
                     ;; could alternately memoize a no-applicable-method call
                     do (when (null am)
                          (error "No applicable methods for SATIATE of ~a with ~a"
                                 generic-function list-of-specializer-names))
                     do (when (and (consp em) (eq (first em) 'no-required-method))
                          (error "No required methods for SATIATE of ~a with ~a"
                                 generic-function list-of-specializer-names))
                     collect `(cons ,(coerce list-of-specializers 'vector)
                                    ,(cond
                                       (standard-slotd-p
                                        (cond (readerp `(make-optimized-slot-reader
                                                         :index ',(slot-definition-location slotd)
                                                         :slot-name ',(slot-definition-name slotd)
                                                         :method ,(ensure-method method)
                                                         :class ,accessor-class))
                                              (writerp `(make-optimized-slot-writer
                                                         :index ',(slot-definition-location slotd)
                                                         :slot-name ',(slot-definition-name slotd)
                                                         :method ,(ensure-method method)
                                                         :class ,accessor-class))
                                              (t (error "BUG: Unreachable weirdness in SATIATE for ~a, ~a"
                                                        generic-function list-of-specializer-names))))
                                       (fmf (ensure-fmf method))
                                       (leafp (ensure-method-function-outcome method))
                                       (t
                                        ;; Force method functions, so that call-method can use them.
                                        (loop for method in am do (ensure-method-function method))
                                        ;; Then the actual EMFO.
                                        (ensure-emfo am em)))))))
        `(let (,@(mapcar #'rest method-binds))
           (let (,@(mapcar #'rest method-function-binds))
             (macrolet ((call-method (method &optional nexts)
                          (if (consp method)
                              (second method) ; (make-method form)
                              (let ((mfs '(,@(loop for (method name) in method-function-binds
                                                   collect (cons method name)))))
                                (flet ((ensure-mf (method)
                                         (or (cdr (assoc method mfs))
                                             (error "BUG: Horrible things are occurring."))))
                                  `(funcall ,(ensure-mf method) .method-args.
                                            (list
                                             ,@(loop for next in nexts
                                                     collect (if (consp next)
                                                                 `(lambda (.method-args. .next-methods.)
                                                                    (declare (ignore .next-methods.))
                                                                    ,(second next))
                                                                 (ensure-mf next))))))))))
               (let (,@(mapcar #'rest fmf-binds))
                 (let (,@(mapcar #'rest method-function-outcome-binds))
                   (let (,@(mapcar #'rest emfo-binds))
                     (list ,@entries)))))))))))

;;; Macro version of SATIATE, that the exported function sometimes expands into.
(defmacro %satiate (generic-function-name &rest lists-of-specializer-names)
  `(let ((gf (fdefinition ',generic-function-name)))
     (append-generic-function-call-history
      gf
      (satiated-call-history ,generic-function-name ,@lists-of-specializer-names))
     #+(or)
     (set-funcallable-instance-function
      gf
      ,(apply #'compile-time-discriminator
              (fdefinition generic-function-name)
              lists-of-specializer-names))
     ;; put in the actual discriminator
;;;     #+(or)
     (force-dispatcher gf)))

;;; Exported auxiliary version for the common case of wanting to skip recompilations
;;; of shared-initialize etc. Just pass it a list of class designators and it'll fix
;;; up the CLOS initialization functions.
(defun satiate-initialization (&rest class-designators)
  (let ((tail (mapcar #'list class-designators)))
    (apply #'satiate #'initialize-instance tail)
    (apply #'satiate #'reinitialize-instance tail))
  (apply #'satiate #'shared-initialize
         (loop for classd in class-designators
               collect (list classd 'symbol)
               collect (list classd 'cons)
               collect (list classd 'null))))

(define-compiler-macro satiate-initialization (&whole form &rest classdfs &environment env)
  (if (every (lambda (classdf) (constantp classdf env)) classdfs)
      (let* ((classds (mapcar (lambda (classdf) (ext:constant-form-value classdf env)) classdfs))
             (tail (mapcar #'list classds)))
        `(progn
           (%satiate initialize-instance ,@tail)
           (%satiate reinitialize-instance ,@tail)
           (%satiate shared-initialize
                     ,@(loop for classd in classds
                             collect `(,classd symbol)
                             collect `(,classd cons)
                             collect `(,classd null)))))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SATIATION OF SPECIFIC CLOS FUNCTIONS
;;;
;;; Used in boot

;; Like %satiate, but also binds the thing so we get the non-gf expansions.
;; (The binding must be at compile time, hence the weirdness here.)
;; Note that this is finicky: We need the %satiate macroexpander specifically to use the variable.
;; As of now this works, because the macroexpander calls compile-time-discriminator, which calls
;; the things that use the variable. But if there was another layer of macrology there'd be a problem.
(defmacro %early-satiate (generic-function-name &rest lists-of-specializer-names &environment env)
  (let ((*early-satiation* t))
    (macroexpand-1 `(%satiate ,generic-function-name ,@lists-of-specializer-names) env)))

(defmacro satiate-clos ()
  ;; This is the ahead-of-time satiation. If we get as much as possible we can speed startup a bit.
  (labels ((readers-from-slot-description (slot-description)
             (loop for (key arg) on (cdr slot-description) by #'cddr
                   when (eq key :reader)
                     collect arg
                   when (eq key :accessor)
                     collect arg))
           (satiate-readers (slot-descriptions specializerses)
             (loop for slotdesc in slot-descriptions
                   for readers = (readers-from-slot-description slotdesc)
                   nconc (loop for reader in readers
                               collect `(%early-satiate ,reader ,@specializerses)))))
    `(progn
       ,@(satiate-readers +specializer-slots+ '((eql-specializer)
                                                (standard-class) (funcallable-standard-class)
                                                (structure-class) (built-in-class) (core:cxx-class)
                                                (core:derivable-cxx-class) (core:clbind-cxx-class)))
       ;; eql-specializer has only the one special slot, so we just do it manually.
       (%early-satiate eql-specializer-object (eql-specializer))
       ,@(satiate-readers (set-difference +class-slots+ +specializer-slots+)
                          '((standard-class) (funcallable-standard-class)
                            (structure-class) (built-in-class) (core:cxx-class)
                            (core:derivable-cxx-class) (core:clbind-cxx-class)))
       ,@(satiate-readers +standard-method-slots+ '((standard-method)
                                                    (standard-reader-method) (standard-writer-method)))
       ,@(satiate-readers (set-difference +standard-accessor-method-slots+ +standard-method-slots+)
                          '((standard-reader-method) (standard-writer-method)))
       ,@(satiate-readers +slot-definition-slots+
                          '((standard-direct-slot-definition) (standard-effective-slot-definition)))
       ,@(satiate-readers +standard-generic-function-slots+
                          '((standard-generic-function)))
       ;; Writers are done manually since the new-value classes are tricky to sort out
       (macrolet ((satiate-specializer-writer (name &rest types) ; i mean, the types are classes though.
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(eql-specializer standard-class funcallable-standard-class
                                             structure-class built-in-class core:cxx-class
                                             core:clbind-cxx-class core:derivable-cxx-class)
                              nconc (loop for type in types
                                          collect `(,type ,class))))))
         (satiate-specializer-writer specializer-direct-methods null cons)
         (satiate-specializer-writer specializer-direct-generic-functions null cons))
       (macrolet ((satiate-class-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-class funcallable-standard-class
                                             structure-class built-in-class core:cxx-class
                                             core:clbind-cxx-class core:derivable-cxx-class)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-class-writer class-id symbol)
         (satiate-class-writer class-direct-superclasses null cons)
         (satiate-class-writer class-direct-subclasses null cons)
         (satiate-class-writer class-slots null cons)
         (satiate-class-writer class-precedence-list null cons)
         (satiate-class-writer class-direct-slots null cons)
         (satiate-class-writer class-direct-default-initargs null cons)
         (satiate-class-writer class-default-initargs null cons)
         (satiate-class-writer class-finalized-p symbol) ; don't really "unfinalize", so no null
         (satiate-class-writer class-size fixnum)
         (satiate-class-writer class-dependents null cons)
         (satiate-class-writer class-valid-initargs null cons)
         (satiate-class-writer creator core:funcallable-instance-creator core:instance-creator))
       (macrolet ((satiate-method-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-method
                                             standard-writer-method standard-reader-method)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-method-writer method-generic-function standard-generic-function)
         (satiate-method-writer method-plist null cons)
         (satiate-method-writer method-keywords null cons) ; note: why is this being called?
         (satiate-method-writer method-allows-other-keys-p null cons))
       (macrolet ((satiate-slotd-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-direct-slot-definition
                                             standard-effective-slot-definition)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-slotd-writer slot-definition-name symbol)
         (satiate-slotd-writer slot-definition-initform null cons) ; guess at what a form is
         (satiate-slotd-writer slot-definition-initfunction function)
         (satiate-slotd-writer slot-definition-type symbol cons) ; type specifiers
         (satiate-slotd-writer slot-definition-allocation symbol)
         (satiate-slotd-writer slot-definition-initargs null cons)
         (satiate-slotd-writer slot-definition-readers null cons)
         (satiate-slotd-writer slot-definition-writers null cons)
         (satiate-slotd-writer slot-definition-location fixnum cons))
       (macrolet ((satiate-gf-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for type in types collect `(,type standard-generic-function)))))
         (satiate-gf-writer generic-function-method-combination method-combination)
         (satiate-gf-writer generic-function-argument-precedence-order cons)
         (satiate-gf-writer generic-function-methods null cons)
         (satiate-gf-writer generic-function-dependents null cons))
       ;; also done in function-to-method
       (%early-satiate compute-applicable-methods-using-classes
                       (standard-generic-function cons)
                       (standard-generic-function null))
       ;; also done in function-to-method
       (%early-satiate compute-applicable-methods
                       (standard-generic-function cons)
                       (standard-generic-function null))
       ;; also done in function-to-method
       (%early-satiate compute-effective-method
                       (standard-generic-function method-combination cons)
                       (standard-generic-function method-combination null))
       (%early-satiate make-instance (symbol) (standard-class) (funcallable-standard-class))
       (%early-satiate allocate-instance (standard-class) (funcallable-standard-class) (structure-class))
       (%early-satiate add-direct-subclass
                       (standard-class standard-class) (funcallable-standard-class funcallable-standard-class)
                       (built-in-class standard-class) ; for gray streams
                       (structure-class structure-class))
       (%early-satiate validate-superclass
                       (standard-class standard-class) (funcallable-standard-class funcallable-standard-class)
                       (structure-class structure-class) (standard-class built-in-class))
       (macrolet ((satiate-classdefs (&rest classes)
                    (let ((tail (mapcar #'list classes)))
                      `(progn (%early-satiate finalize-inheritance ,@tail)
                              (%early-satiate compute-class-precedence-list ,@tail)
                              (%early-satiate compute-slots ,@tail)
                              (%early-satiate class-name ,@tail)
                              (%early-satiate class-prototype ,@tail)
                              (%early-satiate compute-default-initargs ,@tail)
                              (%early-satiate direct-slot-definition-class ,@tail)
                              (%early-satiate effective-slot-definition-class ,@tail)))))
         (satiate-classdefs standard-class funcallable-standard-class structure-class
                            built-in-class core:derivable-cxx-class core:clbind-cxx-class))
       (%early-satiate compute-effective-slot-definition
                       (standard-class symbol cons) (funcallable-standard-class symbol cons)
                       (structure-class symbol cons))
       (%early-satiate ensure-class-using-class (standard-class symbol) (null symbol))
       (%early-satiate function-keywords (standard-method) (standard-reader-method) (standard-writer-method))
       (%early-satiate add-direct-method
                       (structure-class standard-method) (eql-specializer standard-method)
                       (standard-class standard-method) (funcallable-standard-class standard-method)
                       (standard-class standard-reader-method) (standard-class standard-writer-method)
                       (built-in-class standard-method)
                       (built-in-class standard-writer-method) ; for the new-value argument
                       (funcallable-standard-class standard-reader-method)
                       (funcallable-standard-class standard-writer-method))
       (%early-satiate remove-direct-method
                       (structure-class standard-method) (eql-specializer standard-method)
                       (standard-class standard-method) (funcallable-standard-class standard-method)
                       (standard-class standard-reader-method) (standard-class standard-writer-method)
                       (built-in-class standard-method)
                       (built-in-class standard-writer-method) ; for the new-value argument
                       (funcallable-standard-class standard-reader-method)
                       (funcallable-standard-class standard-writer-method))
       (%early-satiate ensure-generic-function-using-class
                       (standard-generic-function symbol) (null symbol))
       ;; these are obviously not complete, but we can throw em in.
       (macrolet ((partly-satiate-initializations (&rest classes)
                    (let ((tail (mapcar #'list classes)))
                      `(progn
                         (%early-satiate initialize-instance ,@tail)
                         (%early-satiate shared-initialize
                                         ,@(loop for class in classes
                                                 collect `(,class symbol)
                                                 collect `(,class cons)
                                                 collect `(,class null)))
                         (%early-satiate reinitialize-instance ,@tail)))))
         (partly-satiate-initializations
          standard-generic-function standard-method standard-class structure-class
          standard-reader-method standard-writer-method
          standard-direct-slot-definition standard-effective-slot-definition
          eql-specializer method-combination funcallable-standard-class))
       (%early-satiate make-instances-obsolete (standard-class) (funcallable-standard-class) (structure-class)))))
