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

;;; Satiation should occur before any generic function calls are performed, and
;;; involve no generic function calls, therefore.

;;; Note that the accessors for call-history and specializer-profile are not accessors,
;;; they are C++ functions. So they're okay to use without early-accessors.

;;; effective-slot-from-accessor-method, but without gfs
(defun satiation-effective-slot-from-accessor-method (method class)
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
;;; FIXME: Duplication of other code, in this case
;;; compute-effective-method-function-maybe-optimize, sucks.
(defun satiation-compute-effective-method-function (methods specializers)
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
                    (slot (satiation-effective-slot-from-accessor-method
                           first (first specializers))))
               (cmp::make-optimized-slot-reader :index (slot-definition-location slot)
                                                :effective-method-function efm
                                                :slot-name (slot-definition-name slot)
                                                :method first
                                                :class class)))
            ((eq (class-of first) (find-class 'standard-writer-method))
             (let* ((class (second specializers))
                    (slot (satiation-effective-slot-from-accessor-method
                           first (first specializers))))
               (cmp::make-optimized-slot-writer :index (slot-definition-location slot)
                                                :effective-method-function efm
                                                :slot-name (slot-definition-name slot)
                                                :method first
                                                :class class)))
            ((leaf-method-p first)
             (if (fast-method-function first)
                 (cmp::make-fast-method-call :function (fast-method-function first))
                 (method-function first)))
            (t efm)))))

;;; Add fictitious call history entries.
(defun add-satiation-entries (generic-function lists-of-specializers)
  (with-early-accessors (+standard-generic-function-slots+) ; for -method-combination
    (let* ((new-entries
             (loop for specific-specializers in lists-of-specializers
                   for methods = (std-compute-applicable-methods-using-classes
                                  generic-function specific-specializers)
                   ;; Everything should use standard method combination during satiation.
                   ;; FIXME: Once compute-effective-method is changed, we should do the
                   ;; reader/writer optimizations here as well.
                   for effective-method-function = (satiation-compute-effective-method-function
                                                    methods
                                                    specific-specializers)
                   collect (cons (coerce specific-specializers 'vector) effective-method-function))))
      (loop for call-history = (generic-function-call-history generic-function)
            for new-call-history = (append new-entries call-history)
            for exchange = (generic-function-call-history-compare-exchange
                            generic-function call-history new-call-history)
            until (eq exchange new-call-history)))))

(defun satiate-generic-function (generic-function lists-of-specializers)
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

(defun satiate-standard-generic-functions ()
  (macrolet ((satiate-one (gf-name &body lists-of-class-names)
               `(prog2
                    (gf-log ,(concatenate 'string "Satiating " (string gf-name) "%N"))
                    (satiate-generic-function
                     (fdefinition ',gf-name)
                     (list ,@(loop for list in lists-of-class-names
                                   collect `(list ,@(loop for name in list
                                                          collect `(find-class ',name))))))
                  (gf-log ,(concatenate 'string "Done satiating " (string gf-name) "%N")))))
    ;; I think what we need to satiate are just what dispatch-miss can call.
    ;; With the actual classes. Abstract classes aren't relevant.
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
(defun satiation-setup-specializer-profile (proto-gf)
  ;; proto-gf in that it's unnaturally defined. it's still a standard-generic-function object.
  (with-early-accessors (+standard-generic-function-slots+
                         +standard-method-slots+)
    (unless (slot-boundp proto-gf 'lambda-list)
      (error "In satiation-setup-specializer-profile - ~s has no lambda list!"
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
          (error "In satiation-setup-specializer-profile - ~s has no methods!"
                 (core:low-level-standard-generic-function-name proto-gf))))
    (gf-log "Set initial specializer profile for satiated function %s to %s%N"
            (core:low-level-standard-generic-function-name proto-gf)
            (generic-function-specializer-profile proto-gf))))

(export '(satiate-standard-generic-functions))

(defmacro satiated-call-history (generic-function-name &rest lists-of-specializer-names)
  (let* ((generic-function (fdefinition generic-function-name))
         (mc (generic-function-method-combination generic-function))
         (methods (generic-function-methods generic-function))
         ;; alist (method . (name method-form mf-name mf-form))
         (bindmap (loop for method in methods
                        for name = (gensym "METHOD")
                        for mf-name = (gensym "METHOD-FUNCTION")
                        collect (list method
                                      name
                                      ;; FIXME: We use FDEFINITIOn instead of #' because we have
                                      ;; local macros for method-qualifiers etc, which are also satiated.
                                      `(find-method (fdefinition ',generic-function-name)
                                                    ',(method-qualifiers method)
                                                    (list ,@(loop for s in (method-specializers method)
                                                                  collect (etypecase s
                                                                            (eql-specializer
                                                                             `(intern-eql-specializer
                                                                               ',(eql-specializer-object s)))
                                                                            (class s)))))
                                      mf-name
                                      `(method-function ,name))))
         (emfo-binds nil) ; alist (applicable-methods . (name form))
         (fmf-binds nil) ; alist (method . (name form))
         (entries
           (loop for list-of-specializer-names in lists-of-specializer-names
                 for list-of-specializers = (mapcar (lambda (sname)
                                                      (etypecase sname
                                                        ;; (eql 'something)
                                                        ((cons (eql eql)
                                                               (cons (cons (eql quote)
                                                                           (cons t null))
                                                                     null))
                                                         ;; The fake EQL specializers used by c-a-m-u-s
                                                         (list (cadadr sname)))
                                                        (symbol (find-class sname))))
                                                    list-of-specializer-names)
                 for am = (compute-applicable-methods-using-specializers generic-function list-of-specializers)
                 for em = (compute-effective-method generic-function mc am)
                 ;; time to recapitulate compute-effective-method-function-maybe-optimize.
                 for method = (and (consp em) (eq (first em) 'call-method) (second em))
                 for leafp = (when method (leaf-method-p method))
                 for fmf = (when leafp (fast-method-function method))
                 for readerp = (when method (eq (class-of method) (find-class 'standard-reader-method)))
                 for writerp = (when method (eq (class-of method) (find-class 'standard-writer-method)))
                 for accessor-class = (cond ((not method) nil)
                                            (readerp (first list-of-specializers))
                                            (writerp (second list-of-specializers))
                                            (t nil))
                 for slotd = (when accessor-class (effective-slotd-from-accessor-method method accessor-class))
                 for standard-slotd-p = (when slotd
                                          (eq (class-of slotd)
                                              (find-class 'standard-effective-slot-definition)))
                 ;; could alternately memoize a no-applicable-method call
                 do (when (null am)
                      (error "No applicable methods for SATIATE of ~a with ~a"
                             generic-function list-of-specializer-names))
                 do (when (and (consp em) (eq (first em) 'no-required-method))
                      (error "No requried methods for SATIATE of ~a with ~a"
                             generic-function list-of-specializer-names))
                 collect `(cons ,(coerce list-of-specializers 'vector)
                                ,(cond
                                   (standard-slotd-p
                                    (cond (readerp `(cmp::make-optimized-slot-reader
                                                     :index ',(slot-definition-location slotd)
                                                     :slot-name ',(slot-definition-name slotd)
                                                     :method ,(or (second (assoc method bindmap)) (error "wtf"))
                                                     :class ,accessor-class))
                                          (writerp `(cmp::make-optimized-slot-writer
                                                     :index ',(slot-definition-location slotd)
                                                     :slot-name ',(slot-definition-name slotd)
                                                     :method ,(or (second (assoc method bindmap)) (error "wtf"))
                                                     :class ,accessor-class))
                                          (t (error "Unreachable weirdness in SATIATE for ~a, ~a"
                                                    generic-function list-of-specializer-names))))
                                   (fmf (let ((group (assoc method fmf-binds)))
                                          (if group
                                              (second group)
                                              (let ((fmf-name (gensym "FAST-METHOD-FUNCTION"))
                                                    (fmf-form `(cmp::make-fast-method-call
                                                                :function
                                                                (fast-method-function
                                                                 ,(or (second (assoc method bindmap))
                                                                      (error "wtf"))))))
                                                (push (list method fmf-name fmf-form) fmf-binds)
                                                fmf-name))))
                                   (leafp (or (fourth (assoc method bindmap)) (error "wtf")))
                                   (t (let ((existing (assoc am emfo-binds :test #'equal)))
                                        (if existing
                                            (second existing)
                                            (let ((name (gensym "EFFECTIVE-METHOD-OUTCOME")))
                                              (push (list am name
                                                          `(cmp::make-effective-method-outcome
                                                            :applicable-methods (list ,@(loop for m in am
                                                                                              collect (or (second (assoc m bindmap)) (error "wtf"))))
                                                            :function (lambda (.method-args. .next-methods.)
                                                                        (declare (ignore .next-methods.))
                                                                        ,em)))
                                                    emfo-binds)
                                              name)))))))))
    `(let (,@(loop for (method name method-form mf-name mf-form) in bindmap
                   collect `(,name ,method-form)))
       (let (,@(loop for (method name method-form mf-name mf-form) in bindmap
                     collect `(,mf-name ,mf-form)))
         (declare (ignorable ,@(mapcar #'fourth bindmap)))
         (let (,@(loop for (method fmf-name fmf-form) in fmf-binds
                       collect `(,fmf-name ,fmf-form)))
           (declare (ignorable ,@(mapcar #'second bindmap)))
           (macrolet ((call-method (method &optional nexts &environment env)
                        (if (consp method)
                            (second method) ; (make-method form)
                            (let ((mfs '(,@(loop for (method name method-form mf-name) in bindmap
                                                 collect (cons method mf-name)))))
                              (flet ((ensure-mf (method)
                                       (or (cdr (assoc method mfs)) (error "Horrible things are occurring."))))
                                `(funcall ,(ensure-mf method) .method-args.
                                          (list ,@(loop for next in nexts
                                                        collect (if (consp next)
                                                                    `(lambda (.method-args. .next-methods.)
                                                                       (declare (ignore .next-methods.))
                                                                       ,(second next))
                                                                      (ensure-mf next))))))))))
             (let (,@(loop for (methods name emfo-form) in emfo-binds
                           collect `(,name ,emfo-form)))
               (list ,@entries))))))))

(defmacro satiate (generic-function-name &rest lists-of-specializer-names)
  `(let ((new-call-history (satiated-call-history ,generic-function-name ,@lists-of-specializer-names))
         (generic-function (fdefinition ',generic-function-name)))
     (loop for call-history = (generic-function-call-history generic-function)
           for exchange = (generic-function-call-history-compare-exchange
                           generic-function call-history new-call-history)
           until (eq exchange new-call-history))))

(defmacro satiate-from-experience (generic-function-name)
  (let* ((generic-function (fdefinition generic-function-name))
         (ch (generic-function-call-history generic-function))
         (lists (loop for (vec . outcome) in ch
                      collect (map 'list (lambda (entry)
                                           (if (listp entry)
                                               `(eql ',(car entry))
                                               (class-name entry)))
                                   vec))))
    `(satiate ,generic-function-name ,@lists)))
